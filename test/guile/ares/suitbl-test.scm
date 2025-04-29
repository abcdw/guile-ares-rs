(define-module (ares suitbl-test)
  #:use-module (ares guile prelude)
  #:use-module (ares suitbl))



;;;
;;; Tests for is, test-case, test-suite that we can use to test test runners
;;;

(comment
 (display 'hi)
 (+ 1 2)
 'hi)

(define-test-suite is-usage-test-suite
  (test-case "basic atomic values"
    (is #t)
    (is 123)
    (is 'some-symbol))

  (test-case "an expression asserting atmoic value of the variable"
    (let ((a 123))
      (is a)))

  (test-case "predicates"
    (is (= 1 1))
    (is (even? 14)))

  (test-case "nested is and is return value"
    (is (= 7 (is (+ 3 4)))))

  ;; Do we need xfail/xpass?
  ;; We can implement it also with test-case metadata and custom test runner
  (test-case "expected to fail"
    ;; Remove not, when it implemented correctly (starts to pass)
    (is (not now-it-false-but-later-will-become-true))))

(define-test-suite test-case-usage-test
  (test-case "simple test case with meta"
    #:metadata `((expected-to-fail . #t))
    (is #f)
    (is #t)))

(define-test-suite test-runner-test-suite
  (is-usage-test-suite))

(define-test-suite execution-timeout-test-suite
  ;; https://legacy.cs.indiana.edu/~dyb/pubs/engines.pdf
  (is #f))

(define (run-tests)
  (test-runner-test-suite))
