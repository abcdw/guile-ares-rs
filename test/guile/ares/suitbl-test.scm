(define-module (ares suitbl-test)
  #:use-module (ares guile prelude)
  #:use-module (ares suitbl)
  #:use-module (ice-9 exceptions))





;; TODO: [Andrew Tropin, 2025-04-22] Check if we need to implement
;; scheduling of test-cases or running them immediately is ok
(define (get-silent-test-runner)
  (define (test-runner x)
    "Default test runner"
    (let ((msg-type (assoc-ref x 'type)))
      (case msg-type
        ((schedule-test-case)
         (let ((test-case-thunk (assoc-ref x 'test-case-thunk)))
           (test-case-thunk)))
        ((run-assert)
         (let ((assert-thunk (assoc-ref x 'assert-thunk))
               (assert-quoted-form (assoc-ref x 'assert-quoted-form)))
           (if (%test-case*)
               (default-run-assert assert-thunk #f assert-quoted-form)
               (test-case
                "anonymous"
                (default-run-assert assert-thunk #f assert-quoted-form)))))

        (else #t))))
  test-runner)

(define (get-simple-test-runner)
  (define (simple-test-runner message)
    "Very simple test runner, just returns the result of @code{is} assert."
    (define (simple-run-assert form-thunk args-thunk quoted-form)
      (with-exception-handler
       (lambda (ex)
         'error)
       (lambda ()
         (let* ((result (form-thunk)))
           (if result 'pass 'fail)))
       #:unwind? #t))

    (let ((msg-type (assoc-ref message 'type)))
      (case msg-type
        ((schedule-test-case)
         (let ((test-case-thunk (assoc-ref message 'test-case-thunk)))

           (test-case-thunk)))

        ((run-assert)
         (let ((assert-thunk (assoc-ref message 'assert-thunk))
               (assert-quoted-form (assoc-ref message 'assert-quoted-form)))
           (simple-run-assert assert-thunk #f assert-quoted-form)))

        (else
         (raise-exception
          (make-exception-with-message
           (format #f "~a handling is not implemented by simple test runner"
                   (assoc-ref message 'type))))))))
  simple-test-runner)

(define-syntax ignore-current-output-port
  (lambda (stx)
    (syntax-case stx ()
      ((_ body body* ...)
       #'(parameterize ((test-runner-output-port* (open-output-string)))
           body body* ...)))))

(reset-test-environment
 get-simple-test-runner
 (is
  (ignore-current-output-port
   (reset-test-environment
    (@@ (ares suitbl) default-get-test-runner)
    (test-case "test"
      (is 123))))))



;;;
;;; Tests for is, test-case, test-suite that we can use to test test runners
;;;

(comment
 (display 'hi)
 (+ 1 2)
 'hi)

(define-test-suite is-usage
  (test-case "is outside of test-case"
    (is
     (throws-exception?
      (reset-test-environment
       get-silent-test-runner
       (test-suite "sample test suite"
         (is (= 7 (+ 3 4))))))))

  (test-case "basic atomic values"
    (is #t)
    (is 123)
    (is 'some-symbol))

  (test-case "an expression asserting atmoic value of the variable"
    (let ((a 123))
      (is a))
    (define b 'heyhey)
    (is b))

  (test-case "predicates"
    (is (= 1 1))
    (is (even? 14))
    (is (lset= = '(1 2 2 3) '(2 3 4 5)))
    (is (= 4 (+ 2 2))))

  ;; TODO: [Andrew Tropin, 2025-05-01] Move to reporter tests
  (test-case "error message is good"
    ;; TODO: [Andrew Tropin, 2025-04-08] Produce more sane error message
    ;; for cases with atomic or identifier expressions.
    (is #f)
    (is (= 4 7))
    (is (lset= = '(1 2 2 3) '(2 3 4 5)))
    (is (= 40000000000000000000000000
           (+ 20000000000000000000000000
              20000000000000000000000000))))

  (test-case "nested is and is return value"
    (is (= 7 (is (+ 3 4))))))

(define-test-suite test-case-usage
  (test-case "simple test case with metadata"
    #:metadata `((expected-to-fail . #t))
    (is #f))

  (test-case "Zero asserts test case"
    "Not yet implemented"))

(define-test-suite nested-test-suites-and-test-cases
  ;; Nested testsuits requires double parentesis to be immediately
  ;; called on evaluation
  (test-case "throws programming-error on unbound variable"
    (is (throws-exception? (+ b 1 2) programming-error?)))

  (test-case "nested test cases are forbidden"
    (is
     (throws-exception?
      (reset-test-environment
       get-silent-test-runner
       (test-case
           "case1"
         (test-case "nested case" (is #t))))
      (lambda (ex)
        (string=? "Test Cases can't be nested"
                  (exception-message ex))))))

  (test-case "test suite nested in test case is forbidden"
    (is
     (throws-exception?
      (reset-test-environment
       get-silent-test-runner
       (test-case
           "case1"
         ((test-suite "nested suite" (is #t)))))
      (lambda (ex)
        (string=? "Test Suite can't be nested into Test Case"
                  (exception-message ex))))))

  ((test-suite "test suite 1"
     (test-case "test case 1#1"
       (is #t)
       (is "very true"))
     ((test-suite "test suite 1.1"
        (test-case "test case 1.1#1"
          (is (= 4 (+ 2 2)))))))))

(define-test-suite test-suite-usage
  "description here?"
  (nested-test-suites-and-test-cases))

(define-test-suite base-test-runner
  (is-usage)
  (test-case-usage)
  (test-suite-usage))

(define-test-suite execution-timeout
  ;; https://legacy.cs.indiana.edu/~dyb/pubs/engines.pdf
  (is #f))

(define-public (run-tests)
  (base-test-runner))

;; TODO: [Andrew Tropin, 2025-04-29] Implement test runner message
;; run-suite or run-suites, which executes test suite and returns
;; execution summary
;; TODO: [Andrew Tropin, 2025-04-30] Implement schedule-and-run-tests
;; message type


;; TODO: [Andrew Tropin, 2025-04-30] Add get-run-summary message type

;; TODO: [Andrew Tropin, 2025-04-29] Add assertion count to test
;; summary

;; TODO: [Andrew Tropin, 2025-04-29] Make assert work without
;; test-case wrap, auto-wrap into test-case is explicit and can be
;; confusing


;; TODO: [Andrew Tropin, 2025-04-29] Separate test runner from
;; reporter.  Reporter could be junit, tap, basic and it's basically
;; almost independent from how tests can be executed. So the test
;; reporter can be specified for a particular test runner to use.

;; TODO: [Andrew Tropin, 2025-04-30] Don't imply reporting, profiling
;; and other logic on test-case macro side, do it inside test runner
;; wrappers

;; TODO: [Andrew Tropin, 2025-04-11] Specify test timeouts to 10 by
;; default, so the test evaluation never hangs.

;; TODO: [Andrew Tropin, 2025-04-11] Make it easy to add tags/metainfo
;; to tests to be able to run/skip them flexibly

;; TODO: [Andrew Tropin, 2025-04-22] Add enable-re-run-failed-tests-on-eval,
;; which will re-run last failed tests on each eval

