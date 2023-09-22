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
  #:use-module (fibers io-wakeup)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (output-stream-manager-thunk
            evaluation-manager-thunk
            evaluation-supervisor-thunk))

;; Managers should be lambdas, because spawn-fiber can have scheduler
;; argument.


;;;
;;; I/O Handling
;;;

(define (read-all-chars-as-string port)
  "Reads all available characters from the PORT and returns a string produced
 out of them."
  (with-output-to-string
    (lambda ()
      (let loop ()
        (when (char-ready? port)
          (write-char (read-char port))
          (loop))))))

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

(define (make-pipes n)
  "Creates a list of N pipes."
  (map (lambda _ (pipe)) (iota n)))

(define (unbuffer-pipes! pipes)
  (for-each (lambda (p) (setvbuf (cdr p) 'none)) pipes)
  pipes)

(define (close-pipes pipes)
  "Takes a list of pipes and close all the related ports."
  (define (close-pipe p)
    ;; the order is important, otherwise it can lead to
    ;; fport_write: Broken pipe
    (close-port (cdr p))
    (close-port (car p)))
  (map (lambda (p) (close-pipe p)) pipes))

(define (call-with-pipes pipes proc)
  (call-with-values
      ;; MAYBE: [Andrew Tropin, 2023-09-06] Handle non-local exit?
      (lambda () (proc pipes))
    (lambda vals
      (close-pipes pipes)
      (apply values vals))))

(define (with-current-ports output-port error-port input-port thunk)
  (parameterize ((current-output-port output-port)
                 (current-error-port error-port)
                 (current-input-port input-port))
    (thunk)))


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
               `((status . interrupted)
                 (exception-value . ,exception)
                 (stack . ,(make-stack #t))))
           (lambda ()
             `((status . done)
               (eval-value . ,(primitive-eval code))))
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
        `(("status" . #("done" "interrupted")))))
     (wrap-operation
      (wait-operation thread-finished-condition)
      (lambda ()
        (let* ((res (join-thread thread))
               (eval-value (assoc-ref res 'eval-value)))
          (if eval-value
              `(("status" . #("done"))
                ("value" . ,eval-value))
              'exception))))))

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

          (put-message replies-channel
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
          ;; Maybe add #:drain? argument to control shutdown behavior
          (shutdown-condition (make-condition))
          (finished-condition (make-condition)))

  (define (get-action command)
    (assoc-ref command 'action))

  ;; We can combine it with other blocking operation to extend the
  ;; possible sources of commands.  For example when thread evaluation
  ;; is finished, we can return a "run-evaluation" command.
  (define recieve-command-operation
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

  (lambda ()
    (let loop ((get-next-command-operation recieve-command-operation)
               (evaluation-finished? (make-condition))
               (evaluation-queue (make-queue)))
      (let* ((command (perform-operation get-next-command-operation))
             (action (assoc-ref command 'action)))
        (format #t "command: ~a\n" command)
        (case action
          ('shutdown
           (signal-condition! finished-condition)
           'finished)
          ;; 'run-evaluation
          ;; 'enqueue evaluation
          ))


      ;; (if command
      ;;     (let ((reply (assoc-ref command 'reply)))
      ;;       (reply 'hi)))

      ;; (if (equal? 'shutdown (get-action command))
      ;;     'finished
      ;;     (loop (get-next-command)))
      )))

;; (let ((x 34))
;;   (interrupt)) -> new nrepl session #2


;; (+ x x) C-2 C-c C-e
