;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2024 Nikita Domnitskii <nikita@domnitskii.me>
;;; Copyright © 2025 Libre en Communs <contact@a-lec.org>
;;; Copyright © 2025 Noé Lopez <noelopez@free.fr>
;;; Copyright © 2010, 2016 Free Software Foundation, Inc.
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

;;; Commentary:

;;; The code in this module is for the evaluation thread. Its purpose
;;; is to execute evaluations synchronously while capturing input,
;;; output, exceptions and return values.

;;; Code:

(define-module (ares evaluation eval)
  #:use-module (ares evaluation io)
  #:use-module (ares reflection modules)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 and-let-star)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module ((system base compile) #:select (read-and-compile))
  #:use-module ((system vm loader) #:select (load-thunk-from-memory))
  #:use-module (fibers channels)
  #:export (evaluation-tag
            evaluation-state
            evaluation-thunk
            evaluation-loop))

(define evaluation-tag (make-prompt-tag "ares-evaluation"))
(define evaluation-state (make-thread-local-fluid 'idle))

(define (evaluation-thunk nrepl-message)
  "Return a thunk, which evaluate code in appropriate module and handle
exceptions."
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
                          (read-and-compile port #:env module
                                            #:optimization-level 0))))
              (call-with-prompt evaluation-tag
                (lambda ()
                  (fluid-set! evaluation-state 'evaluation)
                  (let ((result (start-stack
                                 "ares-evaluation"
                                 (call-with-values thunk
                                   (lambda vals vals)))))
                    (fluid-set! evaluation-state 'idle)
                    result))
                (lambda (_ reply)
                  (fluid-set! evaluation-state 'idle)
                  ;; Reply directly to the interruption request.
                  (reply `(result ((result-type . interrupted))))
                  'interrupted)))))))))

  (define (capture-stack)
    "Captures the current stack without any unwanted frames."
    ;; Adapted from frame->stack-vector.
    ;; (match (fluid-ref %stacks)
    ;;   ((stack-tag . prompt-tag)
    ;;    (make-stack
    ;;     #t
    ;;     ;; Cut three frames from the top of the stack:
    ;;     ;; make-stack, this one, and the throw handler.
    ;;     3
    ;;     ;; Narrow the end of the stack to the most recent start-stack.
    ;;     prompt-tag
    ;;     ;; And one more frame, because %start-stack invoking the
    ;;     ;; start-stack thunk has its own frame too.
    ;;     0 (and prompt-tag 1)))
    ;;    (_
    ;;     ;; Otherwise take the whole stack, except the top three frames.
    ;;     (make-stack #t 3)))

    ;; XXX: The code above for cutting stack frames sometimes return
    ;; #f instead of stack, which makes stack->nrepl-value to fail and
    ;; thus hangs the evaluation session.
    (make-stack #t 3))

  (lambda ()
    ;; file:~/work/gnu/guix/guix/repl.scm::`(exception (arguments ,key ,@(map value->sexp args))
    (let/ec return
      (with-exception-handler
       (lambda (exception)
         (let ((stack (capture-stack)))
           (return `((result-type . exception)
                     (exception-value . ,exception)
                     (stack . ,stack)))))
       (lambda ()
         (match (eval-code)
           ('interrupted
            `((result-type . interrupted)))
           ((val)
            `((result-type . value)
              (eval-value . ,val)))
           (vals
            `((result-type . multiple-values)
              (eval-value . ,vals)))))
       #:unwind? #f))))

(define (apply-evaluation message
                          stdin-channel
                          reply)
  "Applies the evaluation in MESSAGE and returns the
result. In the evaluation context, program output is sent in a call to
REPLY with argument `(output \"message\") or `(error \"message\");
program input is requested by calling REPLY with '(need-input). The
caller can provide input by sending a string on STDIN-CHANNEL."
  (call-with-current-ports
   (evaluation-thunk message)
   #:input
   (open-callback-input-port
            stdin-channel
            (if stdin-channel
                (lambda () (reply '(need-input)))
                (lambda () (error "input requested but no stdin-channel available"))))
   #:output (open-callback-output-port (lambda (str) (reply `(output ,str))))
   #:error (open-callback-output-port (lambda (str) (reply `(error ,str))))
   #:warning (open-callback-output-port (lambda (str) (reply `(error ,str))))))

(define* (evaluation-loop channel
                          #:key
                          (stdin-channel #f))
  "Loop over CHANNEL's messages, reading evaluation requests and sending
the results by calling the message’s REPLY. Read from STDIN-CHANNEL
for standard input.

The messages are alists with at least the following elements:
'((reply . <procedure MESSAGE>)
  (action . <action>))

ACTION can have the following values:
'(evaluate <message>)                   ;evaluate <message>, an nREPL evaluation request
'(quit)                                 ;quit the current evaluation loop"
  (let loop ((frame #f)                 ;latest stack frame in case of recursive evaluation
             (recursion-level 0))
    (let* ((message (get-message channel))
           (action (assoc-ref message 'action))
           (reply (assoc-ref message 'reply)))
      (match action
        (('evaluate message)
         (let* ((result (apply-evaluation message stdin-channel reply)))
           (reply `(result ,result))
           (when (eq? (assq-ref result 'result-type) 'exception)
             (reply
              `(error ,(format #f "Entered recursive evaluation ~a~%" (1+ recursion-level))))
             (loop frame (1+ recursion-level)))
           (loop frame recursion-level)))

        (('quit)
         (unless (= recursion-level 0)
           (reply `(error ,(format #f "Left recursive evaluation ~a~%" recursion-level))))
         *unspecified*)

        (_
         (format (current-error-port)
                 "unknown evaluation loop action ~a~%"
                 message))))))
