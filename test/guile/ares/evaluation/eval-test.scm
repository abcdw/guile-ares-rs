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
  (define (send channel message)
    (quickly (put-operation channel message)))
  (define (test-value name channel expected)
    (test-begin name)
    (let ((reply (quickly (get-operation channel))))
      (test-eq 'result (car reply))
      (test-eq "value" 'value (assq-ref (cadr reply) 'result-type))
      (test-equal "evaluation value" expected (assq-ref (cadr reply) 'eval-value)))
    (test-end name))
  (define (test-exception name channel kind)
    (test-begin name)
    (let ((reply (quickly (get-operation channel))))
      (test-eq 'result (car reply))
      (test-eq "exception" 'exception (assq-ref (cadr reply) 'result-type))
      (test-equal "exception kind" kind (exception-kind (assq-ref (cadr reply) 'exception-value))))
    (test-end name))
  (define (test-message name channel message)
    (test-equal name message (quickly (get-operation channel))))


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
      (test-value "addition" channel 1)
      (send channel '(evaluate (("code" . "(* 1 2 3 4)"))))
      (test-value "multiplication" channel 24)
      (send channel '(evaluate (("code" . "(/ 4935 0)"))))
      (test-exception "enter recursive evaluation" channel 'numerical-overflow)

      (send channel '(evaluate (("code" . "(define a (+ 4 5))"))))
      (test-value "define" channel *unspecified*)
      (send channel '(evaluate (("code" . "a"))))
      (test-value "defined variable" channel 9)

      (send channel '(evaluate (("code" . "(define kont #f)"))))
      (test-value "define" channel *unspecified*)
      (send channel '(evaluate (("code" . "(+ 1 (call/cc (lambda (k) (set! kont k) 5)))"))))
      (test-value "call/cc" channel 6)
      (send channel '(evaluate (("code" . "(kont 41)"))))
      (test-value "calling continuation" channel 42)

      (test-begin "exit recursive evaluation")
      (test-eq #f (quickly (wait-operation stopped-condition)))
      (quickly (put-operation channel '(quit)))
      (test-eq #f (quickly (wait-operation stopped-condition)))
      (quickly (put-operation channel '(quit)))
      (test-eq #t (call-with-values
                      (lambda () (quickly (wait-operation stopped-condition)))
                    (lambda values (null? values))))
      (test-end))))
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
      (test-message "needs input" channel '(need-input))
      (send stdin-channel "(hello world !)")
      (test-value "received input" channel '(hello world !))))))
