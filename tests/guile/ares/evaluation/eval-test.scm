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

(define-module (ares evaluation eval-test)
  #:use-module (ares evaluation eval)
  #:use-module (ares evaluation test-utils)
  #:use-module (srfi srfi-64)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers conditions)
  #:use-module (test-utils))

(define-test test-evaluation-loop
  (define reply-channel (make-channel))
  (define (reply-callback message)
    (put-message reply-channel message))
  (define (send-raw channel message)
    (quickly (put-operation channel message)))
  (define (send channel message)
    (send-raw channel `((reply . ,reply-callback)
                        (action . ,message))))
  (define (check-value name expected)
    (test-group name
      (let ((reply (quickly (get-operation reply-channel))))
        (test-eq 'result (car reply))
        (test-eq "value" 'value (assq-ref (cadr reply) 'result-type))
        (test-equal "evaluation value" expected (assq-ref (cadr reply) 'eval-value)))))
  (define (check-multivalue name expected)
    (test-group name
      (let ((reply (quickly (get-operation reply-channel))))
        (test-eq 'result (car reply))
        (test-eq "mulitple-values" 'multiple-values (assq-ref (cadr reply) 'result-type))
        (test-equal "evaluation value" expected (assq-ref (cadr reply) 'eval-value)))))
  (define (check-exception name kind)
    (test-group name
      (let ((reply (quickly (get-operation reply-channel))))
        (test-eq 'result (car reply))
        (test-eq "exception" 'exception (assq-ref (cadr reply) 'result-type))
        (test-equal "exception kind" kind (exception-kind (assq-ref (cadr reply) 'exception-value))))))
  (define (check-message name message)
    (test-equal name message (quickly (get-operation reply-channel))))


  (test-group
   "Testing Evaluation Loop"
   (run-fibers
    (lambda ()
      (define channel (make-channel))
      (define stopped-condition (make-condition))
      (spawn-fiber (lambda ()
                     (evaluation-loop channel)
                     (signal-condition! stopped-condition)))

      (send channel '(evaluate (("code" . "(- 3 2)"))))
      (check-value "addition" 1)
      (send channel '(evaluate (("code" . "(* 1 2 3 4)"))))
      (check-value "multiplication" 24)
      (send channel '(evaluate (("code" . "(/ 4935 0)"))))
      (check-exception "enter recursive evaluation" 'numerical-overflow)
      (check-message "enter recursive evaluation message"
                    '(error "Entered recursive evaluation 1\n"))

      (send channel '(evaluate (("code" . "(define a (+ 4 5))"))))
      (check-value "define" *unspecified*)
      (send channel '(evaluate (("code" . "a"))))
      (check-value "defined variable" 9)

      (send channel '(evaluate (("code" . "(values 1 2 3)"))))
      (check-multivalue "simple multiple values" '(1 2 3))

      (send channel '(evaluate (("code" . "(define kont #f)"))))
      (check-value "define" *unspecified*)
      (send channel '(evaluate (("code" . "(+ 1 (call/cc (lambda (k) (set! kont k) 5)))"))))
      (check-value "call/cc" 6)
      (send channel '(evaluate (("code" . "(kont 41)"))))
      (check-value "calling continuation" 42)

      (test-group "exit recursive evaluation"
        (test-eq #f (quickly (wait-operation stopped-condition)))
        (send channel '(quit))
        (test-eq #f (quickly (wait-operation stopped-condition)))
        (check-message "left recursive evaluation message"
                       '(error "Left recursive evaluation 1\n"))
        (send channel '(quit))
        (test-eq #t (call-with-values
                        (lambda () (quickly (wait-operation stopped-condition)))
                      (lambda values (null? values))))))))
  (test-group
   "Testing Evaluation Loop ports"
   (run-fibers
    (lambda ()
      (define channel (make-channel))
      (define stdin-channel (make-channel))
      (spawn-fiber
       (lambda ()
         (evaluation-loop channel #:stdin-channel stdin-channel)))

      (send channel '(evaluate (("code" . "(read)"))))
      (check-message "needs input" '(need-input))
      (send-raw stdin-channel "(hello world !)")
      (check-value "received input" '(hello world !))
      (send channel '(evaluate (("code" . "(format #t \"Hello!\")"))))
      (check-message "received stdout" '(output "Hello!"))
      (check-value "write to stdout" #t)))))
