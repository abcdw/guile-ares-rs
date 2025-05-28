;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2024 Nikita Domnitskii <nikita@domnitskii.me>
;;; Copyright © 2025 Libre en Communs <contact@a-lec.org>
;;; Copyright © 2025 Noé Lopez <noelopez@free.fr>
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
  #:export (evaluation-thunk
            evaluation-loop))

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
              (start-stack "ares-evaluation" (thunk)))))))))

  (lambda ()
    ;; file:~/work/gnu/guix/guix/repl.scm::`(exception (arguments ,key ,@(map value->sexp args))
    (let/ec return
      (with-exception-handler
       (lambda (exception)
         (let ((stack
                (make-stack
                 #t
                 ;; Cut three frames from the top of the stack:
                 ;; make-stack, this one, and the throw handler.
                 3)))
           (return `((result-type . exception)
                     (exception-value . ,exception)
                     (stack . ,stack)))))
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
       #:unwind? #f))))

(define* (evaluation-loop channel
                          #:key
                          (stdin-channel #f))
  "Loop over CHANNEL's messages, reading evaluation requests and sending
the results. Read from STDIN-CHANNEL for standard input."
  (let loop ((frame #f)                 ;latest stack frame in case of recursive evaluation
             (recursion-level 0))
    (define action (get-message channel))

    (match action
      (('evaluate message)
       (let* ((result (call-with-current-ports
                       (evaluation-thunk message)
                       #:input (if stdin-channel
                                   (open-channel-input-port channel stdin-channel '(need-input))
                                   (%make-void-port "r"))
                       #:output (open-channel-output-port channel (lambda (str) `(output ,str)))
                       #:error (open-channel-output-port channel (lambda (str) `(error ,str)))
                       #:warning (open-channel-output-port channel (lambda (str) `(error ,str))))))
         (put-message channel `(result ,result))
         (when (eq? (assq-ref result 'result-type) 'exception)
           (put-message
            channel
            `(error ,(format #f "Entered recursive evaluation ~a~%" (1+ recursion-level))))
           (loop frame (1+ recursion-level)))
       (loop frame recursion-level)))
      (('quit)
       (unless (= recursion-level 0)
         (put-message channel
                      `(error ,(format #f "Left recursive evaluation ~a~%" recursion-level))))
       *unspecified*))))
