;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2025 Libre en Communs <contact@a-lec.org>
;;; Copyright © 2025 Noé Lopez <noelopez@free.fr>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (ares guile exceptions)
  #:use-module (ice-9 control)
  #:use-module (ice-9 exceptions)
  #:export (exception->string
            with-exception-string-handler))

(define* (exception->string exception #:optional (frame #f))
  "Returns a string representing the exception. If FRAME is specified,
also contains the source line."
  (call-with-output-string
   (lambda (port)
     (print-exception
      port
      frame
      (exception-kind exception)
      (exception-args exception)))))

(define* (with-exception-string-handler handler thunk #:key (unwind-for-type #f))
  "Calls PROC inside with-exception-handler, on exception HANDLER is
called with a textual representation of the exception. The stack is
unwinded after the call to HANDLER."
  (let/ec return
    (with-exception-handler
     (lambda (exception)
       (return
        (handler
         (exception->string
          exception
          (let ((stack (make-stack #t)))
            ;; First three frames are always make-stack,
            ;; with-exception-string-handler and raise-exception.
            (and (>= (stack-length stack) 4) (stack-ref stack 3)))))))
     thunk
     #:unwind? #f
     #:unwind-for-type unwind-for-type)))
