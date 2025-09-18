;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl reporters)
  #:use-module (ares suitbl runners)
  #:use-module ((ares guile prelude) #:select (comment))

  #:export (throws-exception?)

  #:re-export (test-runner*

               is
               ;; we omit re-exporting -thunk counterparts, as they
               ;; probably never needed by end-user
               test test-thunk
               suite suite-thunk

               define-suite

               test-reporter-output-port*
               test-reporter-silent
               test-reporter-logging
               test-reporter-unhandled
               test-reporter-base
               test-reporter-dots
               test-reporter-dots-with-hierarchy
               test-reporters-use-all
               test-reporters-use-first

               make-suitbl-test-runner))

#|

SUITBL is Scheme Universal Interactive Testing Base Library

The primary tool of the test library is assert macro @code{is}.  You
can use it on it's own and evaluate it, test runner will take care of
executing the body of assert and calling reporter to report result.

The unevaluated form and thunk returning evaluated arguments are
preserved, so it's up to the test report to provide a nice readable
message.  For example if reporter encounters a failure and sees lset=
as a predicate it can say what the difference between sets instead of
using generic message.

Asserts can be grouped with test macro to make a simple unit of
testing with additional documentation string attached to it.  Test can
also be evaluated on it's own, test runner knows what to do with it.

Tests can be grouped into test suites.  Test suites can be nested in
each other.  It allows to combine related tests, build hierarchy,
control the test execution logic: skipping, shuffling, running in
parallel or whatever.

Test suite is a function, it can be executed to load tests defined
inside.  The name of such functions should contain -tests prefix, it's
not a requirement, but a convention to make it easier for the
developer to visually distinguish functions containing tests
inside (aka test suits) from usual functions.

(define addition-tests
  (suite-thunk "addition"
    (test "small numbers addition"
      (is (= 4 (+ 2 2)))
      (is (= 7 (+ 3 4))))

    (test "big numbers addition"
      (is (=    4000000000000
             (+ 2000000000000
                2000000000000))))))

(define subtraction-tests
  (suite-thunk "subtraction"
    (test "small numbers subtraction"
      (is (= 1 (- 4 3)))
      (is (= 3 (- 7 4))))))

(define-public arithmetic-tests
  (suite-thunk "arithmetic"
    (addition-tests)
    (subtraction-tests)))

When you call a test suite, the test runner will build hierarchy of
nested tests and test suites, add it into test runner, later those
loaded tests will be executed.  The order and concurrency of execution
depends on the test runner implementation.

|#


;;;
;;; Auxiliary helpers
;;;

(define-syntax throws-exception?
  (lambda (x)
    (syntax-case x ()
      ((throws-exception? expression)
       #'(throws-exception? expression exception?))
      ((throws-exception? expression predicate)
       #'(with-exception-handler
          (lambda (ex) (predicate ex))
          (lambda ()
            expression
            #f)
          #:unwind? #t)))))

(define-syntax simple-profile
  (lambda (stx)
    (syntax-case stx ()
      ((_ expressions ...)
       #'(let ((start-time (get-internal-real-time))
               (return-value expressions ...))
           (format (test-reporter-output-port*) "run time: ~f\n"
                   (exact->inexact
                    (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
           return-value)))))



#|



|#


