;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023, 2024 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2024 Nikita Domnitskii <nikita@domnitskii.me>
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

(define-module (ares evaluation thread-manager)
  #:use-module (ares reusable-thread)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (ares evaluation io)
  #:use-module (ares evaluation eval)
  #:use-module (ares ports)
  #:use-module (ares guile)
  #:use-module (ares file)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module (ice-9 and-let-star)
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:export (evaluation-thread-manager-thunk))

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
                      (string-append
                       "Origin: " (exception-origin exception) "\n"
                       (exception-message exception) "\n")
                      (or (exception-irritants exception) '())))
              (pretty-print exception #:port port)))))
         (stack (assoc-ref result 'stack)))
    ;; In the future this function can provide more information in a
    ;; more structured way to be processed by respective IDEs/clients.
    `((("err" . ,error)
       ("ares.evaluation/stack" . ,(stack->nrepl-value stack)))
      (("ex" . ,(symbol->string (exception-kind exception)))
       ("status" . #("error" "eval-error" "done")))
      (("status" . #("done"))))))

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

(define (frame->nrepl-value frame)
  "Serializes FRAME into a value that can be sent in nREPL messages."
  (let ((name (symbol->string (or (frame-procedure-name frame) '_)))
        (arguments
         (map (lambda (argument)
                (format #f "~s" argument))
              (frame-arguments frame)))
        (environment
         (map (lambda (binding)
                (match-let (((name . value) binding))
                  (cons name (format #f "~s" value))))
              (frame-environment frame)))
        (source (and-let* ((source (frame-source frame)))
                  `((line . ,(source:line source))
                    (column . ,(source:column source))
                    (file . ,(or (search-in-load-path (source:file source))
                                 (source:file source)))))))
    `((procedure-name . ,name)
      (arguments . ,(list->vector arguments))
      (environment . ,(list->vector environment))
      (source . ,source))))

(define (stack->nrepl-value stack)
  "Serializes STACK into a value that can be sent in nREPL messages."
  (list->vector
   (let loop ((frame (stack-ref stack 0))
              (result '()))
     (if frame
         (loop (frame-previous frame)
               (cons (frame->nrepl-value frame) result))
         result))))
