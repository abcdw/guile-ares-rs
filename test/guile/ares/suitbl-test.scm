(define-module (ares suitbl-test)
  #:use-module (ares guile prelude)
  #:use-module (ares suitbl)
  #:use-module (ares alist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-197)
  #:use-module (ice-9 exceptions))


;;;
;;; Thoughts and Questions
;;;

;;; Tests skipping
;; Do we skip asserts or test cases? test cases, because they are a
;; unit of execution/report/etc, not the assert

;; skipping tests use cases: skip tests for particular platform, test
;; stubs for friend, who will implement the functionality later.

;; Syntax for skipping? can be a xtest, can be
;; (skip-next-test), can be (test "description" 'skip-it (is #f))
;; probably the last one

;; We don't need to bake skipping syntax into test definition, because
;; it bakes test-runner logic into test definitions.  Skipping tests
;; should be done on test-runner side.  What we can add to tests is
;; metainformation.


;;; Expected to pass/fail AKA xpass xfail
;; xpass and xfail is similiar to skipping tests, we can mark some
;; test suites as expected to fail for now and skip them.  Not exactly
;; as xpass/xfail, because we don't see the number of such tests in
;; the result.

;; Other option is to do `(is (not not-yet-trueish-value))' and write
;; a comment that `not' should be removed, when test starts to pass.

;; xfail can be a great way to report a bug and provide a reproducer
;; at the same time

;; this type of workflow can be implemented by extending test runner and
;; adding meta information to test


;;; Basic test run results
;; test results: pass, fail, error, skip


;;; Junit test run summary
#|
https://github.com/testmoapp/junitxml?tab=readme-ov-file#example
name        Name of the entire test run
tests       Total number of tests in this file
failures    Total number of failed tests in this file
errors      Total number of errored tests in this file
skipped     Total number of skipped tests in this file
assertions  Total number of assertions for all tests in this file
time        Aggregated time of all tests in this file in seconds
timestamp   Date and time of when the test run was executed (in ISO 8601 format)
|#


;;; Naming test suites
;; Test suit should contain suffix, which will distinguish it from
;; subject under test.  Even if we export module as sut:, the prefix
;; will make it clearer that it's a test suite, not the function of
;; original module.

#|
Questions:
1.
How to backlink test to function, so you can see all the tests related
to the function?

2.
Test tags, which can be used to produce test suites (subset of tests):
unit, integration, acceptance, backend, frontend
https://github.com/testmoapp/junitxml

3.
Continuous testing.
- Watch for changed tests/implementations?
- Re-run [failed] tests on every eval?

4.
Arguments pre-evaluations, do we really need it?
Maybe it's ok to do post-fail re-evaluation?

Why we pre-evaluate arguments is because we want to produce
meaningful error messages.

Imagine situation:
(let ((a "he")
      (b "hoho"))
  (is (string=? (string-append a "he") b)))

Saying that in expression (string=? (string-append a "he") b)
"hehe" is not string=? to "hoho" is useful, but saying
that (not (string=? (string-append a "he") b)) is not so.

5.
load-tests* variable (or syntax-parameter), which controls macro
expansion logic, setting it to #f will make all test defining
functions to produce empty results.  Probably we don't need it,
because all tests are deffered.  The only possible use case is
stripping out tests from production code, when the tests are in the
same module with the subject under the test.

6.
Skip test functionality.  Do we want a special test-skip
statement or something similiar?  Probably no, because we can skip
test cases on test-runner/IDE side.

|#


;;;
;;; Reference materials
;;;

;; https://gerbil.scheme.org/reference/dev/test.html
;; https://gerbil.scheme.org/reference/std/test.html#test-suite
;; check macro and various convinience wrappers

;; https://srfi.schemers.org/srfi-78/srfi-78.html

;; https://cljdoc.org/d/lambdaisland/kaocha/1.91.1392/doc/5-running-kaocha-from-the-repl
;; https://github.com/weavejester/eftest
;; Nice clojure test runners

;; http://testanything.org/tap-version-14-specification.html
;; test output specification

;; https://github.com/nubank/matcher-combinators
;; A list of helper function, which allows for flexible matching of
;; highly nested data structures



;; TODO: [Andrew Tropin, 2025-04-22] Check if we need to implement
;; scheduling of tests or running them immediately is ok
(define (get-silent-test-runner)
  (define (test-runner x)
    "Default test runner"
    (let ((msg-type (assoc-ref x 'type)))
      (case msg-type
        ((load-test-suite)
         ((assoc-ref x 'load-test-suite-thunk)))

        ((schedule-test)
         (let ((test-thunk (assoc-ref x 'test-thunk)))
           (test-thunk)))
        ((run-assert)
         (let ((assert-thunk (assoc-ref x 'assert-thunk))
               (assert-quoted-form (assoc-ref x 'assert-quoted-form)))
           (assert-thunk)))

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
        ((schedule-test)
         (let ((test-thunk (assoc-ref message 'test-thunk)))

           (test-thunk)))

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
       #'(parameterize ((test-reporter-output-port* (open-output-string)))
           body body* ...)))))
#;
(reset-test-environment
 get-simple-test-runner
 (is
  (ignore-current-output-port
   (reset-test-environment
    (@@ (ares suitbl) default-get-test-runner)
    (test "test"
      (is 123))))))



;; TODO: [Andrew Tropin, 2025-05-01] Make this macro provide useful
;; information to test runner or reporter, so it's easy to understand
;; what went wrong here.
(define-syntax exception-message=?
  (lambda (stx)
    (syntax-case stx ()
      ((_ message expression)
       #'(throws-exception?
          (begin
            expression
            (raise-exception
             (make-exception-with-message
              "expression didn't raise the exception")))
          (lambda (ex)
            (string=? message (exception-message ex))))))))

(comment
 (exception-message=?
  "hello"
  (raise-exception
   (make-exception-with-message "hello"))))

;; TODO: [Andrew Tropin, 2025-05-15] Rename to test-environment-silent
(define-syntax with-silent-test-environment
  (lambda (stx)
    (syntax-case stx ()
      ((_ body body* ...)
       #'(reset-test-environment
          test-runner-create-suitbl
          (parameterize ((test-reporter* test-reporter-silent))
            body body* ...))))))


;;;
;;; Tests for is, test, test-suite that we can use to test test runners
;;;

(comment
 (display 'hi)
 (+ 1 2)
 'hi)


(define-test-suite is-usage-tests
  (test "basic atomic values"
    (is #t)
    (is 123)
    (is 'some-symbol))

  (test "an expression asserting atmoic value of the variable"
    (let ((a 123))
      (is a))
    (define b 'heyhey)
    (is b))

  (test "predicates"
    (is (= 1 1))
    (is (even? 14))
    (is (lset= = '(1 2 2 3) '(1 2 2 3)))
    (is (= 4 (+ 2 2))))

  ;; TODO: [Andrew Tropin, 2025-05-01] Move to reporter tests
  #;
  (test "error message is good"
    ;; TODO: [Andrew Tropin, 2025-04-08] Produce more sane error message
    ;; for cases with atomic or identifier expressions.
    (is #f)
    (is (= 4 7))
    (is (lset= = '(1 2 2 3) '(2 3 4 5)))
    (is (= 40000000000000000000000000
           (+ 20000000000000000000000000
              20000000000000000000000000))))

  (test "if is assert fails when inside test-suite outside of test macro"
    (is
     (throws-exception?
      (with-silent-test-environment
       ((test-suite "sample test suite"
          (is (= 7 (+ 3 4))))))
      (lambda (ex)
        (string=?
         "Assert encountered inside test-suite, but outside of test"
         (exception-message ex))))))

  (test "is on it's own in empty env"
    (is (= 7
           (with-silent-test-environment
            (is 7)))))

  (test "nested is and is return value"
    (is (= 7 (is (+ 3 4))))))

(define-test-suite test-macro-usage-tests
  (test "simple test case with metadata marking it as slow"
    #:metadata `((slow? . #t))
    (sleep 1)
    (is #t))

  (test "zero asserts test macro works fine"
    "Not yet implemented")

  (test "using test macro on its own"
    (define run-summary-with-failures-and-errors
      (with-silent-test-environment
       (test "simple failure"
         (is #f))))

    (is
     (equal?
      '((errors . 0) (failures . 1) (assertions . 1) (tests . 1))
      (alist-select-keys
       '(errors failures assertions tests)
       run-summary-with-failures-and-errors)))))

(define-test-suite nested-test-suites-and-test-macros-tests
  (test "expression throws programming-error on unbound variable"
    (is (throws-exception? (+ b 1 2) programming-error?)))

  (test "nested test macro usage is forbidden"
    (is
     (throws-exception?
      (with-silent-test-environment
       (test "outer test macro"
         (test "nested test macro" (is #t))))
      (lambda (ex)
        (string=? "Test Macros can't be nested"
                  (exception-message ex))))))

  (test "that test-suite nested in test case is forbidden"
    (is
     (throws-exception?
      (with-silent-test-environment
       (test "test macro"
         ((test-suite "nested test-suite" (is #t)))))
      (lambda (ex)
        (string=? "Test Suite can't be nested into Test Macro"
                  (exception-message ex))))))

  ((test-suite "nested test suite 1"
     (test "test macro 1#1"
       (is #t)
       (is "very true"))
     ((test-suite "even more nested test suite 1.1"
        (test "test macro 1.1#1"
          (is (= 4 (+ 2 2)))))))))

(define-test-suite test-suite-usage-tests
  "description here?"

  ;; TODO: [Andrew Tropin, 2025-05-09] Just think about what test
  ;; suite returns, because the expectation that it returns run
  ;; summary, however, it's not the case, when suite is nested

  ;; (define test-suite-results
  ;;   (failing-asserts-tests))
  ;; (pk test-suite-results)
  ;; (test "suite-results"
  ;;   (is (equal? 'hi test-suite-results)))

  (nested-test-suites-and-test-macros-tests))



(define-test-suite test-runner-operations-tests
  (test "\
run summary is #f by default, but appears after test suite is executed"
    (is (equal?
         #f
         (with-silent-test-environment
          ((test-runner-get-current-or-create)
           `((type . get-run-summary))))))

    (is (not
         (null?
          (with-silent-test-environment
           ((test-suite "suite1"
              (test "case1"
                (is #t))))
           ((test-runner-get-current-or-create)
            `((type . get-run-summary)))))))

    (define run-summary-with-failures-and-errors
      (with-silent-test-environment
       ((test-suite "suite"
          (test "simple failure"
            (is #f))
          (test "simple error"
            (is (throw 'hi)))
          (test "error > failure"
            (is #f)
            (is (throw 'hi)))))
       ((test-runner-get-current-or-create)
        `((type . get-run-summary)))))

    (is
     (equal?
      '((errors . 2) (failures . 1) (assertions . 4) (tests . 3))
      (alist-select-keys
       '(errors failures assertions tests)
       run-summary-with-failures-and-errors)))))

(define-test-suite base-test-runner-tests
  (is-usage-tests)
  (test-runner-operations-tests)
  (test-macro-usage-tests)
  (test-suite-usage-tests))

(define-test-suite execution-timeout-tests
  ;; https://legacy.cs.indiana.edu/~dyb/pubs/engines.pdf
  (is #f))

(define-public (run-tests)
  (let* ((test-runner (test-runner-create-suitbl)))
    (parameterize ((test-reporter* test-reporter-dots))
      (test-runner-run-test-suites
       test-runner
       (list base-test-runner-tests)))
    (define summary (test-runner `((type . get-run-summary))))
    (format #t "\n~a\n" summary)

    (define number-of-tests
      (assoc-ref summary 'tests))

    (unless (= 15 number-of-tests)
      (chain "Unexpected number of tests, make sure all tests are executed and
expected number of tests is up-to-date."
             (make-exception-with-message _)
             (raise-exception _)))
    (when (> (+ (assoc-ref summary 'failures) (assoc-ref summary 'errors)) 0)
      (exit 1))))


;; TODO: [Andrew Tropin, 2025-05-12] Add metadata for test suites

;; TODO: [Andrew Tropin, 2025-05-12] Preserve execution information,
;; so we can re-run failed tests

;; TODO: [Andrew Tropin, 2025-05-09] Revisit test-path* usage on test
;; loading, we preserve test suite hierarchy now and it maybe redundant

;; TODO: [Andrew Tropin, 2025-05-09] Describe private/public test
;; suite logic, if you don't want test-suite to be executed by
;; run-project-tests, just don't export it, right?

;; TODO: [Andrew Tropin, 2025-05-09] Make it possible to specify
;; multiple composable reporters

;; TODO: [Andrew Tropin, 2025-05-09] ?Rename test to check as this
;; name suites better the purpose

;; TODO: [Andrew Tropin, 2025-05-01] Add variable
;; test-runner-under-test* and make-clean-test-runner-environment
;; macro, which will reset the environment and use
;; test-runner-under-test* as a test runner for it, to make it easier
;; to define tests for test runner and sometimes customize test
;; runner (for example make it more verbose for debugging purpose).

;; TODO: [Andrew Tropin, 2025-05-05] Revisit test suite thunk logic,
;; who setups the environment via parameterize, who run scheduled
;; tests and do we schedule tests or build test hierarchy (naming
;; kinda important for understanding here) ?

;; TODO: [Andrew Tropin, 2025-04-11] Specify test timeouts to 10 by
;; default, so the test evaluation never hangs.

;; TODO: [Andrew Tropin, 2025-04-22] Add enable-re-run-failed-tests-on-eval,
;; which will re-run last failed tests on each eval

;; TODO: [Andrew Tropin, 2025-05-03] Add SRFI-64 migration tooling?

;; TODO: [Andrew Tropin, 2025-05-01] Return back profiling to test-runner

;; TODO: [Andrew Tropin, 2025-05-12] Add load-tests* syntax parameter,
;; which will control if tests should be evaluated/loaded.

;; TODO: [Andrew Tropin, 2025-05-12] How to access private functions
;; of SUT (subject module under test)?  Rust have nested tests
;; submodule, which lexcally closed over parent module (have access to
;; private vars)
;; https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html

;; TODO: [Andrew Tropin, 2025-05-12] Add documentation tests
;; https://doc.rust-lang.org/rust-by-example/testing/doc_testing.html
;; They get executed with all the usual tests and also get embedded in
;; the documentation
;; https://doc.rust-lang.org/stable/std/ptr/macro.addr_of.html


