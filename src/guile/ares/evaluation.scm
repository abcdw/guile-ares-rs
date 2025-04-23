;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023, 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (ares evaluation)
  #:use-module (ares alist)
  #:use-module (ares ports)
  #:use-module (ares reflection modules)
  #:use-module (ares reusable-thread)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (fibers io-wakeup)
  #:use-module (fibers operations)
  #:use-module (fibers scheduler)
  #:use-module (fibers timers)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-197)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module ((system base compile) #:select (read-and-compile))
  #:use-module ((system repl debug) #:prefix repl-debug:)
  #:use-module ((system vm loader) #:select (load-thunk-from-memory))
  #:export (output-stream-manager-thunk
            evaluation-thread-manager-thunk
            evaluation-supervisor-thunk
            evaluation-supervisor-shutdown
            evaluation-supervisor-process-nrepl-message))


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

(define (evaluation-thunk nrepl-message)
  "Return a thunk, which evaluate code in appropriate module and handle
exceptions."
  (define out (current-output-port))

  (define (eval-code)
    (let* ((code (assoc-ref nrepl-message "code"))
           (ns (assoc-ref nrepl-message "ns"))
           (module
            (or
             (string->resolved-module ns)
             (current-module))))
      (save-module-excursion
       (lambda ()
         (set-current-module module)
         (call-with-input-string
          code
          (lambda (port)
            (and-let* ((file (assoc-ref nrepl-message "file")))
              (set-port-filename! port file))
            (and-let* ((line (assoc-ref nrepl-message "line")))
              (set-port-line! port line))
            (and-let* ((column (assoc-ref nrepl-message "column")))
              (set-port-column! port column))
            (let ((thunk (load-thunk-from-memory
                          (read-and-compile port #:env module))))
              (start-stack "ares-evaluation" (thunk)))))))))

  (lambda ()
    ;; file:~/work/gnu/guix/guix/repl.scm::`(exception (arguments ,key ,@(map value->sexp args))
    (with-exception-handler
     (lambda (exception)
       `((result-type . exception)
         (exception-value . ,exception)
         (stack . ,(make-stack #t))))
     (lambda ()
       (call-with-values eval-code
         (lambda vals
           (match vals
             ((val)
              `((result-type . value)
                (eval-value . ,val)))
             (vals
              `((result-type . multiple-values)
                (eval-value . ,vals)))))))
     #:unwind? #t)))

(define (setup-redirects-for-ports-thunk output-pipes
                                         input-port
                                         thunk-finished-condition
                                         replies-channel
                                         wrap-with-ports-channel
                                         finished-condition)
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
         (wait stderr-finished)
         (signal-condition! finished-condition))))))


;;;
;;; nREPL Helpers
;;;

(define (exception->nrepl-messages result)
  (let* ((exception (assoc-ref result 'exception-value))
         (error
          ;; There are no particular special about this way of printing
          ;; the exception, it's just a simple implementation, which
          ;; usually provides enough information to understand the
          ;; problem.
          (call-with-output-string
           (lambda (port)
             ;; TODO: [Andrew Tropin, 2023-12-17] Cat out a meaninful
             ;; stack part in evaluation thread.

             ;; (false-if-exception
             ;;  (begin
             ;;    (repl-debug:print-frames
             ;;     (repl-debug:stack->vector (assoc-ref result 'stack))
             ;;     port)
             ;;    (newline port)))
             (or
              (false-if-exception
               (apply format port
                      (string-append (exception-message exception) "\n")
                      (or (exception-irritants exception) '())))
              (pretty-print exception #:port port))))))
    ;; In the future this function can provide more information in a
    ;; more structured way to be processed by respective IDEs/clients.
    `((("err" . ,error))
      (("ex" . ,(symbol->string (exception-kind exception)))
       ("status" . #("error" "eval-error" "done")))
      (("status" . #("done"))))))

(define (object->pretty-string x)
  "Same as @code{object->string}, but with @var{printer} set to
@code{pretty-print}.  Last newline produced by @code{pretty-print} is
dropped."
  (string-drop-right
   (object->string x pretty-print)
   ;; pretty-print adds a newline, so we remove it
   1))

(define* (evaluation-result->nrepl-messages
          result
          #:key
          (format-value object->pretty-string))
  (let ((result-type (assoc-ref result 'result-type)))
    (case result-type
      ((value)
       `((("value" . ,(format-value (assoc-ref result 'eval-value)))
          ("status" . #("done")))))
      ((multiple-values)
       (multiple-values->nrepl-messages result format-value))
      ((exception)
       (exception->nrepl-messages result))
      ((interrupted)
       `((("status" . #("done" "interrupted")))))
      (else (error (format #f "unknown result-type: ~a\n" result-type))))))

(define (multiple-values->nrepl-messages result format-value)
  "Returns a few nrepl messages with additional status multiple-values,
the last message doesn't contain the value, it contains only
@code{((\"status\" . (\"done\", \"multiple-values\")))}."
  (let lp ((vals (assoc-ref result 'eval-value))
           (msgs '()))
    (if (null? vals)
        (reverse (cons `(("status" . #("done" "multiple-values"))) msgs))
        (lp (cdr vals)
            (cons `(("value" . ,(format-value (car vals)))
                    ("status" . #("multiple-values")))
                  msgs)))))

(define (interrupt-result->nrepl-messages result)
  (define status (assoc-ref result 'status))
  (case status
    ((done)
     `((("status" . #("done" "interrupted")))))
    ((idle)
     `((("status" . #("done" "session-idle")))))))


;;;
;;; Event Loops
;;;

(define* (evaluation-thread-manager-thunk
          command-channel
          #:key
          (spawn-reusable-thread make-reusable-thread)
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
    ;; https://tonsky.me/blog/unicode/
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
              (evaluation-rethread (spawn-reusable-thread result-channel)))
         (let loop ((reply-channel #f)
                    (evaluation-finished #f)
                    (output-finished-condition #f))
           (define (repeat-loop)
             (loop reply-channel evaluation-finished output-finished-condition))
           (define (reset-loop)
             (loop #f #f #f))

           (define (mark-evaluation-finished!)
             ;; We can't directly signal EVALUATION-THUNK-FINISHED
             ;; inside evaluation thread, because call/cc can restore
             ;; context, where it is not available, so we need to rely
             ;; on RESULT-CHANNEL for understanding if evaluation
             ;; actually finished.
             (signal-condition! evaluation-finished)
             (wait output-finished-condition))

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
           (case action
             ((return-value)
              (let ((value (assoc-ref command 'value)))
                (mark-evaluation-finished!)
                ;; TODO: [Andrew Tropin, 2023-10-18] Add pretty print
                ;; for returning value
                (reply-with-messages (evaluation-result->nrepl-messages value))
                (reset-loop)))

             ((evaluate)
              (let ((nrepl-message (assoc-ref command 'message))
                    (replies-channel (assoc-ref command 'replies-channel))
                    (wrap-with-ports-thunk-channel (make-channel))
                    (output-finished-condition (make-condition))
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
                  wrap-with-ports-thunk-channel
                  output-finished-condition))

                (reusable-thread-discard-and-run
                 evaluation-rethread
                 ((get-message wrap-with-ports-thunk-channel)
                  (evaluation-thunk nrepl-message)))

                (loop replies-channel evaluation-thunk-finished
                      output-finished-condition)))
             ((interrupt)
              (reply-with-messages
               (interrupt-result->nrepl-messages
                (car
                 (reusable-thread-interrupt evaluation-rethread))))
              (mark-evaluation-finished!)
              (reset-loop))
             ((need-input)
              (reply-with-messages
               `((("status" . #("need-input")))))
              (repeat-loop))
             ((provide-input)
              (put-message stdin-channel (assoc-ref command 'stdin))
              (repeat-loop))
             (else
              (error "it sholud not get here")))))))))

(define (replies-manager-thunk replies-channel reply!)
  (lambda ()
    (let loop ()
      ;; TODO: [Andrew Tropin, 2023-09-22] buffer those messages in
      ;; case reply blocks?
      (reply! (get-message replies-channel))
      (loop))))

(define* (evaluation-supervisor-thunk
          control-channel
          #:key
          (pure-dynamic-state (current-dynamic-state))
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

  (define evaluation-thread-command-channel (make-channel))
  (define evaluation-thread-shutdown-condition (make-condition))

  (define (run-evaluation nrepl-message
                          get-next-command-operation
                          evaluation-queue)
    "Starts evaluation, return operation which unblocks on next command
arrival or when evaluation is finished, #t and rest of the queue."
    (let* ((finished-condition (make-condition))
           (replies-channel (make-channel))
           (command (front evaluation-queue))
           (reply! (assoc-ref command 'reply!)))
      (spawn-fiber
       (replies-manager-thunk replies-channel reply!))
      (put-message evaluation-thread-command-channel
                   `((action . evaluate)
                     (message . ,nrepl-message)
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

  (define (spawn-reusable-thread ch)
    (with-dynamic-state pure-dynamic-state
      (lambda ()
        (make-reusable-thread ch))))

  (lambda ()
    (spawn-fiber (evaluation-thread-manager-thunk
                  evaluation-thread-command-channel
                  #:spawn-reusable-thread spawn-reusable-thread
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
        (case action
          ((terminate)
           ;; (signal-condition! evaluation-id)
           ;; (signal-condition! evaluation-thread-terminate-condition)
           (signal-condition! finished-condition)
           'finished)

          ((shutdown)
           (signal-condition! evaluation-thread-shutdown-condition)
           (signal-condition! finished-condition)
           'finished)

          ((finished)
           (loop
            (if (queue-empty? evaluation-queue)
                receive-command-operation
                evaluate-command-operation)
            #f evaluation-queue))

          ((evaluate)
           ;; We can't be here if queue is empty
           (let* ((command (front evaluation-queue))
                  (nrepl-message (assoc-ref command 'message))
                  (reply! (assoc-ref command 'reply!)))
             (apply loop (run-evaluation
                          nrepl-message
                          receive-command-operation
                          ;; It's not obvious, that element is
                          ;; removed from the evaluation-queue
                          evaluation-queue))))

          ((process-nrepl-message)
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
                   (begin
                     ((assoc-ref command 'reply!)
                      `(("status" . #("done"))))
                     (put-message evaluation-thread-command-channel
                                  `((action . interrupt))))
                   ((assoc-ref command 'reply!)
                    `(("status" . #("session-idle" "done")))))
               (loop
                get-next-command-operation
                evaluation-id
                evaluation-queue))

              ((equal? "stdin" op)
               (put-message evaluation-thread-command-channel
                            `((action . provide-input)
                              (stdin . ,(assoc-ref message "stdin"))))
               ((assoc-ref command 'reply!)
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
                                                     reply!)
  (put-message control-channel `((action . process-nrepl-message)
                                 (message . ,message)
                                 (reply! . ,reply!))))

;; (let ((x 34))
;;   (interrupt)) -> new nrepl session #2


;; (+ x x) C-2 C-c C-e
