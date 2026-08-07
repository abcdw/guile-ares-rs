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
  #:use-module (bencode)
  #:use-module (ice-9 control)
  #:use-module (ice-9 eval-string)
  #:use-module ((srfi srfi-1) #:select (alist-delete every))
  #:use-module (srfi srfi-64)
  #:use-module (ares evaluation serialization)
  #:use-module (test-utils))

(define-test test-stack->nrepl-value
  (define basic-stack #f)
  (define error-stack #f)
  (define error-stack-2 #f)
  (define source-without-file-stack #f)

  (define-syntax check-stack
    (syntax-rules ()
      ((check-syntax stack)
       (begin
         (define vec (stack->nrepl-value stack))
         (test-equal (stack-length stack) (vector-length vec))
         (test-assert "Stack response is bencode encodable"
           (string? (scm->bencode-string
                     `(("ares.evaluation/stack" . ,vec)))))
         (test-assert "Environment bindings contain string names and values"
           (every
            (lambda (frame)
              (every (lambda (binding)
                       (and (vector? binding)
                            (= 2 (vector-length binding))
                            (string? (vector-ref binding 0))
                            (string? (vector-ref binding 1))))
                     (vector->list (assoc-ref frame 'environment))))
            (vector->list vec)))
         (define frame (vector-ref vec (1- (vector-length vec))))
         (test-equal "make-stack" (assoc-ref frame 'procedure-name))
         (test-equal #("#t") (assoc-ref frame 'arguments))
         (test-equal "Frame without source omits source field"
                     #f
                     (assq 'source frame))
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
             #:unwind? #f)))
    (set! source-without-file-stack
          ((eval-string
            "(lambda () (let ((stack (make-stack #t))) (display \"\") stack))"
            #:module (current-module)
            #:file #f
            #:compile? #t))))

  (test-group "basic stack" (check-stack basic-stack))
  (test-group "error stack" (check-stack error-stack))
  (test-group "error stack 2" (check-stack error-stack-2))
  (test-group "source without file"
    (define vec (stack->nrepl-value source-without-file-stack))
    (define frame (vector-ref vec (- (vector-length vec) 2)))
    (define source (assoc-ref frame 'source))
    (test-assert "Source field is present" source)
    (test-equal "Source without file omits file field"
                #f
                (assq 'file source))))

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
