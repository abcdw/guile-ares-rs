;;; guile-ares-rs --- Asynchronous Reliable Extensible Scheme RPC Server
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
;;;
;;; This file is part of guile-ares-rs.
;;;
;;; guile-ares-rs is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; guile-ares-rs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guile-ares-rs.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nrepl server evaluation)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (fibers io-wakeup)
  #:use-module (fibers operations)
  #:use-module (fibers timers)
  #:use-module (fibers scheduler)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (nrepl alist)
  #:use-module (nrepl atomic)
  #:use-module (nrepl ports)
  #:export (output-stream-manager-thunk
            reusable-thread-thread
            evaluation-manager-thunk
            evaluation-thread-manager-thunk
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
;; one aggregating actor with non-trivial synchronization logic.
(define* (output-stream-manager-thunk input-port
                                      wrap-function
                                      replies-channel
                                      process-finished-condition
                                      #:key
                                      (finished-condition (make-condition)))
  "Watches INPUT-PORT and when something arrives reads it as a string,
wraps with WRAP-FUNCTION and sends to the REPLIES-CHANNEL.  Works
until PROCESS-FINISHED-CONDITION is signaled or INPUT-PORT is closed.
Signals FINISHED-CONDITION, when it is completed."
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
            (signal-condition! finished-condition))))))


;;;
;;; Eval Thread
;;;

(define-record-type <reusable-thread>
  (%make-reusable-thread thread nutex result-channel)
  reusable-thread?
  (thread reusable-thread-thread)
  (nutex reusable-thread-nutex)
  (result-channel reusable-thread-result-channel))

(define (make-nutex)
  "A syncronization point, similiar to unowned mutex, but it created
already locked and works without suspending the whole thread."
  (make-atomic-box (make-condition)))

(define (nutex-lock! nutex)
  (define new-condition (make-condition))
  (define (update-condition old-condition)
    (wait old-condition)
    new-condition)
  (atomic-box-update! nutex update-condition))

(define (nutex-unlock! nutex)
  (signal-condition! (atomic-box-ref nutex)))

(define (nutex-wait nutex)
  (wait (atomic-box-ref nutex)))

(define (rthread-lock-or-get-value! rthread)
  (define nutex (reusable-thread-nutex rthread))
  (define result-channel (reusable-thread-result-channel rthread))
  (define new-condition (make-condition))

  (define (value-or-new-condition old-condition)
    (perform-operation
     (choice-operation
      (wrap-operation
       (wait-operation old-condition)
       (const new-condition))
      (get-operation result-channel))))

  (let loop ((old-value (atomic-box-ref nutex)))
    (define new-value (value-or-new-condition old-value))
    (if (eq? new-condition new-value)
        (let ((cas-value
               (atomic-box-compare-and-swap! nutex old-value new-value)))
          (if (eq? old-value cas-value)
              new-value ; lock acquired
              (loop cas-value)))

        ;; Value returned
        new-value)))

(define thread-entry-point-tag (make-prompt-tag "thread-entry-point"))

(define (command-loop-thunk nutex result-channel)
  (lambda ()
    ;; (define suspending-mutex (make-mutex))
    ;; (lock-mutex suspending-mutex)

    (define (suspending-thunk)
      ;; (lock-mutex suspending-mutex)
      (sleep 100)
      (nutex-lock! nutex)
      (abort-to-prompt thread-entry-point-tag `((action . shutdown))))

    (define (return-value value)
      (put-message result-channel value))

    (let loop ((thunk #f))
      (call-with-prompt thread-entry-point-tag
        (lambda ()
          ;; (if thunk
          ;;     (format #t "=> scheduling: ~a\n" thunk)
          ;;     (format #t "=> waiting for interrupt\n"))
          (nutex-unlock! nutex)
          (define thunk-value ((or thunk suspending-thunk)))
          ;; it either locked or the thread is interrupted
          (nutex-lock! nutex)
          (abort-to-prompt
           thread-entry-point-tag
           `((action . return-value)
             (value . ,thunk-value))))

        (lambda (k command)
          (define action (assoc-ref command 'action))

          (cond
           ((equal? 'return-value action)
            (return-value command))
           ((equal? 'interrupt action)
            (return-value `((action . interrupt)
                            (status . ,(if thunk 'done 'idle))))))

          (cond
           ((equal? 'run action)
            (loop (assoc-ref command 'thunk)))
           ((equal? 'shutdown action) 'finished)
           (else (loop #f))))))))

(define* (make-reusable-thread result-channel)
  "Starts a thread and makes sure it entered restart prompt."
  (let* ((nutex (make-nutex))
         (thread
          (call-with-new-thread (command-loop-thunk nutex result-channel))))
    (nutex-wait nutex)
    ;; (spawn-fiber (lambda ()
    ;;                ;; We can't directly wait for condition variable in
    ;;                ;; thread because when asyncs does non-local exit
    ;;                ;; the thread is not removed from condition
    ;;                ;; variable waiters.
    ;;                (wait shutdown-condition)
    ;;                (unlock-mutex shutdown-mutex)))
    (%make-reusable-thread thread nutex result-channel)))

(define (%reusable-thread-do reusable-thread thunk)
  "Schedule a thunk for execution on REUSABLE-THREAD.  This a helper
function, don't use it directly, it doesn't guarantee that nutex will
be unlocked."
  (let loop ((replies '()))
    (define lock-or-value (rthread-lock-or-get-value! reusable-thread))
    (if (condition? lock-or-value)
        (begin
          (system-async-mark
           thunk
           (reusable-thread-thread reusable-thread))
          (reverse replies))
        (loop (cons lock-or-value replies)))))

(define (reusable-thread-get-value-operation reusable-thread)
  (get-operation (reusable-thread-result-channel reusable-thread)))

(define (reusable-thread-get-value reusable-thread)
  (perform-operation (reusable-thread-get-value-operation reusable-thread)))

(define (command->thunk command)
  (lambda ()
    (abort-to-prompt thread-entry-point-tag command)))

(define (%reusable-thread-schedule-command reusable-thread command)
  (%reusable-thread-do reusable-thread (command->thunk command)))

(define* (reusable-thread-discard-and-run reusable-thread thunk)
  "Interrupt and reuse REUSABLE-THREAD with a new THUNK.  It pauses all
current computations, starts a THUNK and after THUNK is finished
aborts to thread entry point tag, which cancels all previous
computations."
  (%reusable-thread-schedule-command
   reusable-thread
   `((action . run)
     (thunk . ,thunk))))

(define* (reusable-thread-shutdown reusable-thread)
  (%reusable-thread-schedule-command
   reusable-thread
   `((action . shutdown))))

(define* (reusable-thread-interrupt reusable-thread)
  (append
   (%reusable-thread-schedule-command
    reusable-thread
    `((action . interrupt)))
   (list (reusable-thread-get-value reusable-thread))))

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

(define (evaluation-thunk code)
  "Return a thunk, which evaluate CODE and handle exceptions."
  (define out (current-output-port))
  (lambda ()
    ;; file:~/work/gnu/guix/guix/repl.scm::`(exception (arguments ,key ,@(map value->sexp args))
    (with-exception-handler
        (lambda (exception)
          `((result-type . exception)
            (exception-value . ,exception)
            (stack . ,(make-stack #t))))
      (lambda ()
        `((result-type . value)
          (eval-value . ,((@ (system base compile) compile)
                          code #:env (current-module)))))
      #:unwind? #t)))


(define* (evaluation-result-manager-thunk
          result-channel replies-channel
          #:key
          (pretty-print (lambda (x) (format #f "~s" x))))
  "Obtains a result from RESULT-CHANNEL, converts it to nrepl messages
and passes to REPLIES-CHANNEL."


  (lambda ()
    (for-each
     (lambda (reply) (put-message replies-channel reply))
     (evaluation-result->nrepl-messages
      (get-message result-channel)))))

(define (setup-redirects-for-ports-thunk output-pipes
                                         input-port
                                         thunk-finished-condition
                                         replies-channel
                                         wrap-with-ports-channel)
  "Returns a thunk, which setups redirects for ports, spawning respective
output stream managers and wait until they finished.  After everything
is set, puts a wrap-with-ports function into WRAP-WITH-PORTS-CHANNEL.
Stream managers waits until THUNK-FINISHED is signalled."

  (define (wrap-output-with tag)
    "Return a function, which wraps argument into alist."
    (lambda (v) `((,tag . ,v))))

  (lambda ()
    (match output-pipes
      ;; Destructure a list of 2 pipes into 4 separate variables
      (((stdout-input-port . stdout-output-port)
        (stderr-input-port . stderr-output-port))
       (let ((wrap-with-ports (lambda (thunk)
                                (lambda ()
                                  (with-current-ports
                                   stdout-output-port
                                   stderr-output-port
                                   input-port
                                   thunk))))
             (stdout-finished (make-condition))
             (stderr-finished (make-condition)))
         ;; TODO: [Andrew Tropin, 2023-09-06] Add input-stream-manager
         ;; use custom or soft ports?

         (spawn-fiber
          (output-stream-manager-thunk stdout-input-port
                                       (wrap-output-with "out")
                                       replies-channel
                                       thunk-finished-condition
                                       #:finished-condition stdout-finished))
         (spawn-fiber
          (output-stream-manager-thunk stderr-input-port
                                       (wrap-output-with "err")
                                       replies-channel
                                       thunk-finished-condition
                                       #:finished-condition stderr-finished))

         (put-message wrap-with-ports-channel wrap-with-ports)
         (wait stdout-finished)
         (wait stderr-finished))))))

(define* (evaluation-result->nrepl-messages
          result
          #:key
          (pretty-print (lambda (x) (format #f "~s" x))))
    (let ((result-type (assoc-ref result 'result-type)))
      (cond
       ((equal? 'value result-type)
        `((("value" . ,(pretty-print (assoc-ref result 'eval-value)))
           ("status" . #("done")))))
       ((equal? 'exception result-type)
        (exception->replies (assoc-ref result 'exception-value)))
       ((equal? 'interrupted result-type)
        `((("status" . #("done" "interrupted")))))
       (else (error (format #f "unknown result-type: ~a\n" result-type))))))

(define (interrupt-result->nrepl-messages result)
  (define status (assoc-ref result 'status))
  (cond
   ((equal? 'done status)
    `((("status" . #("done" "interrupted")))))
   ((equal? 'idle status)
    `((("status" . #("done" "session-idle")))))))

(define* (evaluation-thread-manager-thunk
          command-channel
          #:key
          ;; TODO: [Andrew Tropin, 2023-10-16] Implement shutdown
          (shutdown-condition (make-condition))
          (terminate-condition (make-condition)))
  "Spawn a thread and can run/interrupt evaluation on it via commands sent to
COMMAND-CHANNEL."

  (define (open-channel-input-port request-channel input-channel)
    (define buffer "")
    (define position 0)
    (define (length)
      (string-length buffer))
    (define (code-points-left)
      (- (length) position))

    (define (read! dst start count)
      (when (= 0 (code-points-left))
        (put-message request-channel `((action . need-input)))
        (set! buffer (get-message input-channel))
        (set! position 0))
      (let ((count (min count (code-points-left))))
        (string-copy! dst start buffer position (+ position count))
        (set! position (+ position count))
        count))

    (make-custom-textual-input-port "channel-port" read! #f #f #f))

  (lambda ()
    (call-with-pipes
     (unbuffer-pipes! (make-pipes 2))
     (lambda (pipes)
       (let* ((result-channel (make-channel))
              (input-request-channel (make-channel))
              (stdin-channel (make-channel))
              (input-port (open-channel-input-port
                           input-request-channel
                           stdin-channel))
              (evaluation-rethread (make-reusable-thread result-channel)))
         (let loop ((reply-channel #f)
                    (evaluation-finished #f))
           (define (repeat-loop)
             (loop reply-channel evaluation-finished))
           (define (reset-loop)
             (loop #f #f))

           (define (reply-with-messages messages)
             (for-each
              (lambda (m)
                (put-message reply-channel m))
              messages))

           (define command
             (perform-operation
              (choice-operation
               (get-operation input-request-channel)
               (get-operation result-channel)
               (get-operation command-channel))))

           (define action (assoc-ref command 'action))

           ;; (format #t "command=> ~y" command)
           (cond
            ((equal? 'return-value action)
             (let ((value (assoc-ref command 'value)))
               (reply-with-messages (evaluation-result->nrepl-messages value))
               ;; We can't directly signal EVALUATION-THUNK-FINISHED
               ;; inside evaluation thread, because call/cc can restore
               ;; context, where it is not available, so we need to rely
               ;; on RESULT-CHANNEL for understanding if evaluation
               ;; actually finished.
               (signal-condition! evaluation-finished)
               (reset-loop)))
            ((equal? 'evaluate action)
             (let ((code (assoc-ref command 'code))
                   (replies-channel (assoc-ref command 'replies-channel))
                   (wrap-with-ports-thunk-channel (make-channel))
                   (evaluation-thunk-finished
                    (assoc-ref command 'evaluation-finished)))

               (or evaluation-thunk-finished
                   (error "evaluation-finished is not provided"))
               (spawn-fiber
                (setup-redirects-for-ports-thunk
                 pipes
                 input-port
                 evaluation-thunk-finished
                 replies-channel
                 wrap-with-ports-thunk-channel))

               (reusable-thread-discard-and-run
                evaluation-rethread
                ((get-message wrap-with-ports-thunk-channel)
                 (evaluation-thunk code)))

               (loop replies-channel evaluation-thunk-finished)))
            ((equal? 'interrupt action)
             (reply-with-messages
              (interrupt-result->nrepl-messages
               (car
                (reusable-thread-interrupt evaluation-rethread))))
             (signal-condition! evaluation-finished)
             ;; (reuse-thread evaluation-rethread (lambda () 'interrupted))
             (reset-loop))
            ((equal? 'need-input action)
             (reply-with-messages
              `((("status" . #("need-input")))))
             (repeat-loop))
            ((equal? 'provide-input action)
             (put-message stdin-channel (assoc-ref command 'stdin))
             (repeat-loop))
            (else
             (error "it sholud not get here")))))))))

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

  (define evaluation-thread-command-channel (make-channel))
  (define evaluation-thread-shutdown-condition (make-condition))

  (define (run-evaluation code get-next-command-operation evaluation-queue)
    "Starts evaluation, return operation which unblocks on next command
arrival or when evaluation is finished, #t and rest of the queue."
    (let* ((finished-condition (make-condition))
           (replies-channel (make-channel))
           (command (front evaluation-queue))
           (reply (assoc-ref command 'reply)))
      (spawn-fiber
       (replies-manager-thunk replies-channel reply))
      (put-message evaluation-thread-command-channel
                   `((action . evaluate)
                     (code . ,code)
                     (replies-channel . ,replies-channel)
                     (evaluation-finished . ,finished-condition)))

      (list
       (choice-operation
        (wrap-operation
         (wait-operation finished-condition)
         (const `((internal? #t)
                  (action . finished))))
        get-next-command-operation)
       (or (alist-get-in '(message "id") command) "0")
       (dequeue evaluation-queue))))

  (lambda ()
    (spawn-fiber (evaluation-thread-manager-thunk
                  evaluation-thread-command-channel
                  #:shutdown-condition evaluation-thread-shutdown-condition))
    (let loop ((get-next-command-operation receive-command-operation)
               (evaluation-id #f)
               (evaluation-queue (make-queue)))
      (let* ((command (perform-operation get-next-command-operation))
             (action (assoc-ref command 'action))
             (repeat-loop (lambda ()
                            (loop get-next-command-operation
                                  evaluation-id
                                  evaluation-queue))))
        ;; (format #t "_________\ncommand: ~yevaluation-id: ~a\n\n"
        ;;         ;; get-next-command-operation
        ;;         command evaluation-id)
        (cond
         ((equal? 'terminate action)
          ;; (signal-condition! evaluation-id)
          ;; (signal-condition! evaluation-thread-terminate-condition)
          (signal-condition! finished-condition)
          'finished)

         ((equal? 'shutdown action)
          (signal-condition! evaluation-thread-shutdown-condition)
          (signal-condition! finished-condition)
          'finished)

         ((equal? 'finished action)
          (loop
           (if (queue-empty? evaluation-queue)
               receive-command-operation
               evaluate-command-operation)
           #f evaluation-queue))

         ((equal? 'evaluate action)
          ;; We can't be here if queue is empty
          (let* ((command (front evaluation-queue))
                 (code-string (alist-get-in '(message "code") command))
                 (reply (assoc-ref command 'reply))
                 (read-result (try-read reply code-string)))
            (if read-result
                (apply loop (run-evaluation
                             (assoc-ref read-result 'code)
                             receive-command-operation
                             ;; It's not obvious, that element is
                             ;; removed from the evaluation-queue
                             evaluation-queue))
                (loop
                 receive-command-operation
                 evaluation-id
                 (dequeue evaluation-queue)))))

         ((equal? 'process-nrepl-message action)
            (let* ((message (assoc-ref command 'message))
                   (op (assoc-ref message "op")))
              (cond
               ((equal? "eval" op)
                (loop
                 (if evaluation-id
                     get-next-command-operation
                     evaluate-command-operation)
                 evaluation-id
                 (enqueue evaluation-queue command)))

               ((equal? "interrupt" op)
                (if evaluation-id
                    ;; TODO: [Andrew Tropin, 2023-09-26] Add
                    ;; interrupt-id-mismatch.
                    ;; https://github.com/nrepl/nrepl/blob/master/src/clojure/nrepl/middleware/session.clj#L344
                    (put-message evaluation-thread-command-channel
                                 `((action . interrupt)))
                    ((assoc-ref command 'reply)
                     `(("status" . #("session-idle" "done")))))
                (loop
                 get-next-command-operation
                 evaluation-id
                 evaluation-queue))

               ((equal? "stdin" op)
                (put-message evaluation-thread-command-channel
                             `((action . provide-input)
                               (stdin . ,(assoc-ref message "stdin"))))
                ((assoc-ref command 'reply)
                 `(("status" . #("done"))))
                (repeat-loop))

               (else
                (format (current-error-port)
                        "What is going on in evaluation supervisor: ~a\n"
                        message)
                (error "kawabanga"))))))))))

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
