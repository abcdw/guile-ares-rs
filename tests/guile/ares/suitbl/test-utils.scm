;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl test-utils)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl runners)
  #:use-module ((ares suitbl runner-state)
                #:select (get-scheduled-tests
                          get-runner-config))
  #:export (make-test-runner-with-mixed-tests
            runner->state
            test-descriptions
            scheduled-descriptions))

(define (make-test-runner-with-mixed-tests)
  "Create a runner with a mix of slow and fast tests loaded."
  (define tr (make-silent-test-runner))
  (with-test-runner tr
    (suite "mixed tests"
      (test "fast addition"
        (is (= 4 (+ 2 2))))
      (test "slow network call" 'metadata '((slow? . #t))
        (is #t))
      (test "fast string check"
        (is (string? "hello")))
      (test "slow database query" 'metadata '((slow? . #t))
        (is #t))))
  tr)

(define (runner->state runner)
  (runner `((type . runner/get-state))))

(define (test-descriptions tests)
  "Extract descriptions from a list of test alists."
  (map (lambda (t) (assoc-ref t 'test/description)) tests))

(define (scheduled-descriptions runner)
  "Get the descriptions of all scheduled tests for RUNNER."
  (let ((state (runner->state runner)))
    (test-descriptions
     (get-scheduled-tests state (get-runner-config state)))))
