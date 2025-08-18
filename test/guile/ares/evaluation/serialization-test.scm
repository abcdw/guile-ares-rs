;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
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

(define-module (ares evaluation serialization-test)
  #:use-module (test-utils)
  #:use-module (ice-9 control)
  #:use-module ((srfi srfi-1) #:select (alist-delete))
  #:use-module (srfi srfi-64)
  #:use-module (ares evaluation serialization))

(define-test test-stack->nrepl-value
  (define basic-stack #f)
  (define error-stack #f)
  (define error-stack-2 #f)

  (define-syntax check-stack
    (syntax-rules ()
      ((check-syntax stack)
       (begin
         (define vec (stack->nrepl-value stack))
         (test-equal (stack-length stack) (vector-length vec))
         (define frame (vector-ref vec (1- (vector-length vec))))
         (test-equal "make-stack" (assoc-ref frame 'procedure-name))
         (test-equal #("#t") (assoc-ref frame 'arguments))
         (if #f #f)))))

  (test-group "initialization"
    (set! basic-stack (make-stack #t))
    (set! error-stack
          (let/ec return
            (with-exception-handler
             (lambda (exception) (return (make-stack #t)))
             (lambda () (raise-exception #f))
             #:unwind? #f)))
    (set! error-stack-2
          (let/ec return
            (with-exception-handler
             (lambda (exception) (return (make-stack #t)))
             (lambda () (alist-delete #f 'a))
             #:unwind? #f))))

  (test-group "basic stack" (check-stack basic-stack))
  (test-group "error stack" (check-stack error-stack))
  (test-group "error stack 2" (check-stack error-stack-2)))

(define-test test-interrupt-result->nrepl-messages
  (test-group "interrupt result"
    (test-equal `((("status" . #("done" "interrupted"))))
                (interrupt-result->nrepl-messages
                 `((action . interrupt)
                   (status . done))))
    (test-equal `((("status" . #("done" "session-idle"))))
                (interrupt-result->nrepl-messages
                 `((action . interrupt)
                   (status . idle))))))
