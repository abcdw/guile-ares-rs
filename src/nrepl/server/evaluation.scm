;;; guile-nrepl --- Asyncronous Reliable Extensible Scheme Network REPL
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
;;;
;;; This file is part of guile-nrepl.
;;;
;;; guile-nrepl is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; guile-nrepl is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guile-nrepl.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nrepl server evaluation)
  #:use-module (fibers)
  #:use-module (fibers operations)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (fibers timers)
  #:use-module (fibers io-wakeup)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (nrepl alist)
  #:use-module (nrepl ports)
  #:export (output-stream-manager-thunk
            evaluation-manager-thunk
            evaluation-supervisor-thunk
            evaluation-supervisor-shutdown
            evaluation-supervisor-process-nrepl-message))

;; Managers should be lambdas, because spawn-fiber can have scheduler
;; argument.


;;;
;;; Helpers
;;;

(define (exception->replies exception)
  (let ((error (apply format #f
                      (string-append (exception-message exception) "\n")
                      (caddr (exception-args exception)))))
    `((("err" . ,error))
      (("ex" . ,(symbol->string (exception-kind exception)))
       ("status" . #("eval-error")))
      (("status" . #("done"))))))

(define (reply-with-exception reply exception)
  (for-each reply (exception->replies exception)))


;;;
;;; I/O Handling
;;;

;; Do channel accepts nrepl messages or just strings?  Strings can
;; help to delay nrepl related logic further up the stack.  But will
;; require additional actor to wrap messages for each output port or
;; one aggregating actor with non-trivial syncronization logic.
(define (output-stream-manager-thunk input-port
                                     wrap-function
                                     replies-channel
                                     process-finished-condition)
  "Watches INPUT-PORT and when something arrives reads it as a string,
wraps with WRAP-FUNCTION and sends to the REPLIES-CHANNEL.  Works
until PROCESS-FINISHED-CONDITION is signaled or INPUT-PORT is closed."
  (define (port-open? port) (not (port-closed? port)))
  (lambda ()
    (let loop ()
      (let ((op-value
             (perform-operation
              (choice-operation
               (wrap-operation
                (wait-until-port-readable-operation input-port)
                (const 'ready))
               (wrap-operation
                (wait-operation process-finished-condition)
                (const 'finished))))))

        ;; Try to read anyway, in case something came before process finished
        (when (and (port-open? input-port) (char-ready? input-port))
          (put-message replies-channel
                       (wrap-function (read-all-chars-as-string input-port))))

        ;; It doesn't make sense to keep watching port if it's already closed
        (if (and (equal? 'ready op-value) (port-open? input-port))
            (loop)
            'finished)))))


;;;
;;; Eval Thread
;;;

(define (make-evaluation-thread code finished-condition)
  "Evaluate code in a separate thread.  Signals FINISHED-CONDITION
 even if thread is cancelled."
  (call-with-new-thread
   (lambda ()
     (dynamic-wind
       (const #t)
       (lambda ()
         ;; file:~/work/gnu/guix/guix/repl.scm::`(exception (arguments ,key ,@(map value->sexp args))
         (with-exception-handler
             (lambda (exception)
               `((exception-value . ,exception)
                 (stack . ,(make-stack #t))))
           (lambda ()
             `((eval-value . ,(primitive-eval code))))
           #:unwind? #t))
       (lambda () (signal-condition! finished-condition))))))

(define* (evaluation-manager-thunk code replies-channel
                                   #:key
                                   (interrupt-condition (make-condition))
                                   (finished-condition (make-condition)))
  "Evaluates the CODE in non-blocking way, sends stdout, stderr,
evaluation result messages to REPLIES-CHANNEL.  Evaluation can be
interrupted by signaling INTERRUPT-CONDITION.  When evaluation
finished the FINISHED-CONDITION is signalled by evaluation-manager."

  (define (get-thread-value-operation thread
                                      interrupt-condition
                                      thread-finished-condition)
    (choice-operation
     (wrap-operation
      (wait-operation interrupt-condition)
      (lambda ()
        (cancel-thread thread)
        ;; TODO: [Andrew Tropin, 2023-09-07] Maybe
        ;; join-thread is needed here to ensure that the
        ;; thread interruption is complete.
        `((("status" . #("done" "interrupted"))))))
     (wrap-operation
      (wait-operation thread-finished-condition)
      (lambda ()
        (let* ((res (join-thread thread))
               (eval-value (assoc-ref res 'eval-value)))
          (if (assoc 'eval-value res)
              ;; TODO: [Andrew Tropin, 2023-09-25] Pass
              ;; format/pprint to evaluation-manager-thunk
              `((("value" . ,(format #f "~s" eval-value))
                 ("status" . #("done"))))
              (exception->replies (assoc-ref res 'exception-value))))))))

  (define (wrap-output-with tag)
    "Return a function, which wraps argument into alist."
    (lambda (v) `((,tag . ,v))))

  (lambda ()
    (call-with-pipes ; Ensure pipes are closed
     (unbuffer-pipes! (make-pipes 3))
     (match-lambda
       ;; Destructure a list of 3 pipes into 6 separate variables
       (((stdout-input-port . stdout-output-port)
         (stderr-input-port . stderr-output-port)
         (stdin-input-port . stdin-output-port))
        (let* ((thread-finished-condition (make-condition))
               (output-channel (make-channel))
               (error-channel (make-channel))

               (eval-thread-thunk
                (lambda ()
                  (make-evaluation-thread code thread-finished-condition)))

               (evaluation-thread (with-current-ports
                                   stdout-output-port
                                   stderr-output-port
                                   stdin-input-port
                                   eval-thread-thunk))

               (thread-value-operation
                (get-thread-value-operation evaluation-thread
                                            interrupt-condition
                                            thread-finished-condition)))

          ;; TODO: [Andrew Tropin, 2023-09-06] Add input-stream-manager
          ;; use custom or soft ports?
          (spawn-fiber
           (output-stream-manager-thunk stdout-input-port
                                        (wrap-output-with "out")
                                        replies-channel
                                        thread-finished-condition))

          (spawn-fiber
           (output-stream-manager-thunk stderr-input-port
                                        (wrap-output-with "err")
                                        replies-channel
                                        thread-finished-condition))

          (for-each
           (lambda (reply) (put-message replies-channel reply))
           (perform-operation thread-value-operation))
          (signal-condition! finished-condition)))))))

(define (replies-manager-thunk replies-channel reply)
  (lambda ()
    (let loop ()
      ;; TODO: [Andrew Tropin, 2023-09-22] buffer those messages in
      ;; case reply blocks?
      (reply (get-message replies-channel))
      (loop))))

(define* (evaluation-supervisor-thunk
          control-channel
          #:key
          ;; shutdown is graceful operation
          (shutdown-condition (make-condition))
          (finished-condition (make-condition)))

  (define (get-action command)
    (assoc-ref command 'action))

  ;; We can combine it with other blocking operation to extend the
  ;; possible sources of commands.  For example when thread evaluation
  ;; is finished, we can return a "run-evaluation" command.
  (define receive-command-operation
    (choice-operation
     (wrap-operation
      (wait-operation shutdown-condition)
      (lambda ()
        `((internal? . #t)
          (action . shutdown))))
     (wrap-operation
      (get-operation control-channel)
      (lambda (x)
        (alist-cons 'internal? #f x)))))

  (define (make-queue)
    '())

  (define (enqueue queue object)
    (cons object queue))

  (define (dequeue queue)
    (drop-right queue 1))

  (define (front queue)
    (last queue))

  (define (queue-empty? queue)
    (null? queue))

  (define (make-signaled-condition)
    (let ((condition (make-condition)))
      (signal-condition! condition)
      condition))

  (define evaluate-command-operation
    (wrap-operation
     (sleep-operation 0)
     (const `((internal? . #t)
              (action . evaluate)))))

  (define (try-read reply code)
    (with-exception-handler
        (lambda (exception)
          (reply-with-exception reply exception)
          #f)
      (lambda ()
        (list (cons 'code (with-input-from-string code read))))
      #:unwind? #t))

  (define (run-evaluation code get-next-command-operation evaluation-queue)
    "Starts evaluation, return operation which unblocks on next command or
evaluation finish, interrupt-condition and rest of the queue."
    (let* ((finished-condition (make-condition))
           (interrupt-condition (make-condition))
           (replies-channel (make-channel))
           (command (front evaluation-queue))
           (reply (assoc-ref command 'reply)))
      (spawn-fiber
       (replies-manager-thunk replies-channel reply))
      (spawn-fiber
       (evaluation-manager-thunk
        code replies-channel
        #:interrupt-condition interrupt-condition
        #:finished-condition finished-condition))
      interrupt-condition
      (list
       (choice-operation
        (wrap-operation
         (wait-operation finished-condition)
         (const `((internal? #t)
                  (action . finished))))
        get-next-command-operation)
       interrupt-condition
       (dequeue evaluation-queue))))

  (lambda ()
    (let loop ((get-next-command-operation receive-command-operation)
               (interrupt-condition #f)
               (evaluation-queue (make-queue)))
      (let* ((command (perform-operation get-next-command-operation))
             (action (assoc-ref command 'action)))
        ;; (format #t "operation: ~s\ncommand: ~s\ninterrupt: ~a\n\n"
        ;;         get-next-command-operation
        ;;         command interrupt-condition)
        (cond
         ((equal? 'terminate action)
          (signal-condition! interrupt-condition)
          (signal-condition! finished-condition)
          'finished)

         ((equal? 'shutdown action)
           (signal-condition! finished-condition)
           'finished)

         ((equal? 'finished action)
          (loop
           (if (queue-empty? evaluation-queue)
               receive-command-operation
               evaluate-command-operation)
           #f evaluation-queue))

         ((equal? 'evaluate action)
          (let* ((command (front evaluation-queue))
                 (code-string (alist-get-in '(message "code") command))
                 (reply (assoc-ref command 'reply))
                 (read-result (try-read reply code-string)))
            (if read-result
                (apply loop (run-evaluation
                             (assoc-ref read-result 'code)
                             receive-command-operation
                             evaluation-queue))
                (loop
                 receive-command-operation
                 interrupt-condition
                 (dequeue evaluation-queue)))))

         ((equal? 'process-nrepl-message action)
            (let* ((message (assoc-ref command 'message))
                   (op (assoc-ref message "op")))
              (cond
               ((equal? "eval" op)
                (loop
                 (if interrupt-condition
                     get-next-command-operation
                     evaluate-command-operation)
                 interrupt-condition
                 (enqueue evaluation-queue command)))

               ((equal? "interrupt" op)
                (if interrupt-condition
                    ;; TODO: [Andrew Tropin, 2023-09-26] Add
                    ;; interrupt-id-mismatch.
                    ;; https://github.com/nrepl/nrepl/blob/master/src/clojure/nrepl/middleware/session.clj#L344
                    (signal-condition! interrupt-condition)
                    ((assoc-ref command 'reply)
                     `(("status" . #("session-idle" "done")))))
                (loop
                 get-next-command-operation
                 interrupt-condition
                 evaluation-queue))

               (else
                (format (current-error-port)
                        "What is going on in evaluation supervisor: ~a\n"
                        message)
                (throw 'kawabanga))))))))))

(define (evaluation-supervisor-shutdown control-channel)
  (put-message control-channel '((action . shutdown))))

(define (evaluation-supervisor-process-nrepl-message control-channel
                                                     message
                                                     reply)
  (put-message control-channel `((action . process-nrepl-message)
                                 (message . ,message)
                                 (reply . ,reply))))

;; (let ((x 34))
;;   (interrupt)) -> new nrepl session #2


;; (+ x x) C-2 C-c C-e
