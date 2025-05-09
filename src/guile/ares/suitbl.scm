;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 exceptions)
  #:use-module (ares atomic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-197)
  #:export (test-reporter-output-port*
            test-reporter*
            test-reporter-silent
            test-reporter-base
            test-reporter-dots

            create-suitbl-test-runner
            schedule-and-run-test-suits
            get-current-or-create-test-runner

            define-test-suite
            test-suite test is

            reset-test-environment
            throws-exception?))


#|

SUITBL is Scheme Universal Interactive Testing Base Library

The primary tool of the test library is assert macro @code{is}.  You
can use it on it's own and evaluate it, test runner will take care of
executing the body of assert and calling reporter to report result.

Asserts can be grouped with test macro to make a simple unit of
testing.  Test can also be evaluated on it's own, test runner knows
what to do with it.

Tests can be grouped into test suites.  Test suits can be nested in
each other.  It allows to combine related tests, build hierarchy,
control the test execution logic, skipping, shuffling, whatever.

Test suite is a function, it can be executed to load tests defined
inside.  The name of such functions should contain -tests prefix, it's
not a requirement, but a convention to make it easier for the
developer to distinguish functions containing tests inside.

(define-test-suite addition-tests
  (test "small numbers addition"
    (is (= 4 (+ 2 2)))
    (is (= 7 (+ 3 4)))))

(define-test subtraction-tests
  (test "small numbers subtraction"
    (is (= 1 (- 4 3)))
    (is (= 3 (- 7 4)))))

(define-test arithmetic-tests
  (addition-tests)
  (subtraction-tests))

When you call a test suite, the test runner will build hierarchy of
nested tests and test suits add it into test runner, later those
loaded tests will be executed.  The order and concurrency of execution
depends on the runner implementation.

|#


;;;
;;; Reporters
;;;

(define test-reporter-output-port* (make-parameter (current-output-port)))

(define (test-reporter-base message)
  (define (string-repeat s n)
    "Returns string S repeated N times."
    (fold
     (lambda (_ str)
       (string-append str s))
     ""
     (iota n)))

  (define msg-type (assoc-ref message 'type))
  (case msg-type
    ((test-suite-enter)
     (format (test-reporter-output-port*)
             (string-append
              (string-repeat "|" (length (%test-path*)))
              "┌"))
     (format (test-reporter-output-port*)
             "> ~a\n" (assoc-ref message 'description)))
    ((test-suite-leave)
     (format (test-reporter-output-port*)
             (string-append
              (string-repeat "|" (length (%test-path*)))
              "└"))
     (format (test-reporter-output-port*)
             "> ~a\n" (assoc-ref message 'description)))

    ((test-scheduled)
     (format (test-reporter-output-port*)
             (string-repeat "|" (length (%test-path*))))
     (format (test-reporter-output-port*)
             " + test ~a\n" (assoc-ref message 'description)))

    ((test-start)
     (format (test-reporter-output-port*)
             "\n┌Test ~a\n" (assoc-ref message 'description)))
    ((test-end)
     (format (test-reporter-output-port*)
             "└Test ~a\n" (assoc-ref message 'description)))

    ((assert-pass)
     (format (test-reporter-output-port*) "✓ ~s\n"
             (assoc-ref message 'quoted-form)))

    ((assert-fail)
     (format (test-reporter-output-port*) "✗ Expected: ~s\n   Actual:   ~s\n"
             (assoc-ref message 'quoted-form) (assoc-ref message 'result)))

    ((assert-error)
     (format (test-reporter-output-port*) "✗ ~s produced error:\n   ~s\n"
             (assoc-ref message 'quoted-form) (assoc-ref message 'error)))

    (else
     (raise-exception
      (make-exception-with-message
       (format #f "no reporting implemented for message type ~a" msg-type))))))

(define (test-reporter-silent message)
  #f)

(define (test-reporter-dots message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type
    ((test-suite-enter)
     (format (test-reporter-output-port*) "["))
    ((test-suite-leave)
     (format (test-reporter-output-port*) "]"))

    ((test-start)
     (format (test-reporter-output-port*) "("))
    ((test-end)
     (format (test-reporter-output-port*) ")"))

    ((assert-pass)
     (format (test-reporter-output-port*) "."))
    ((assert-fail)
     (format (test-reporter-output-port*) "F"))
    ((assert-error)
     (format (test-reporter-output-port*) "E"))

    ((test-scheduled)
     (format (test-reporter-output-port*) "T"))

    (else
     (raise-exception
      (make-exception-with-message
       (format #f "no reporting implemented for message type ~a" msg-type))))))

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

(define test-reporter* (make-parameter test-reporter-base))


;;;
;;; Test runners
;;;

#|

Contains information:
- The capture of stdout/stderr.

- the structure suite1.suite2.suite3.case1.assert1

- for every assert it will contain the result and time start/end

- for every case it will contain the summary on the number of asserts,
number of fails and successes, total time spent.

- similiar for test suite.


Life cycle:

We can either create a test runner and pass it a list of tests to
execute or asserts, tests and test-suites can instantiate some
default test-runner.


Operations on test runner:
begin-suite (time)
begin-case (time)

begin-assert (time)
end-assert (time, pass or fail)

end-case (time)
end-suite (time)

(message-test-runner
 (get-current-test-runner)
 `((type . run-test-suites)
   (test-suites . ,(get-list-of-test))))

(message-test-runner
 (get-current-test-runner)
 `((type . test-suite-start)
   (path . (list "suite 1" "suite 2"))
   (time . ,(current-time))))

(message-test-runner
 (get-current-test-runner)
 `((type . get-run-result)))

What `is` does, when it executed on its own? Does it create a test
runner and ask it to execute itself?

|#

(define %test-events* (make-parameter #f))

(define (default-run-assert form-thunk args-thunk quoted-form)
  (with-exception-handler
   (lambda (ex)
     (when (%test*)
       (atomic-box-update!
        (%test-events*)
        (lambda (value)
          (cons 'error value))))
     ((test-reporter*)
      `((type . assert-error)
        (quoted-form . ,quoted-form)
        (error . ,ex))))
   (lambda ()
     ;; TODO: [Andrew Tropin, 2024-12-23] Write down evaluation time
     ;; TODO: [Andrew Tropin, 2024-12-23] Report start before evaling the form
     (let* ((result (form-thunk)))
       (when (%test*)
         (atomic-box-update!
          (%test-events*)
          (lambda (value)
            (cons (if result 'pass 'fail) value))))
       ((test-reporter*)
        `((type . ,(if result 'assert-pass 'assert-fail))
          (result . ,result)
          (quoted-form . ,quoted-form)))
       result))
   #:unwind? #t))


(define (run-test test-thunk)
  (parameterize ((%test-events* (make-atomic-box '())))
    ;; TODO: [Andrew Tropin, 2025-04-24] Handle exceptions that can
    ;; happen inside test case, but outside of assert
    (test-thunk)
    (atomic-box-ref (%test-events*))))

(define (run-scheduled-tests tests)
  ;; pass, fail, skip(?), error
  (let loop ((errors 0)
             (failures 0)
             (assertions 0)
             (tests 0)
             (remaining-tests tests))
    (if (null? remaining-tests)
        `((errors . ,errors)
          (failures . ,failures)
          (assertions . ,assertions)
          (tests . ,tests))
        (begin
          (let* ((result (run-test (caar remaining-tests)))
                 (error? (any (lambda (x) (eq? x 'error)) result))
                 (fail? (any (lambda (x) (eq? x 'fail)) result))
                 (inc-if (lambda (condition value)
                           (if condition (1+ value) value))))
            ;; (pk failed)
            (loop
             (inc-if error? errors)
             (inc-if (and fail? (not error?)) failures)
             (+ assertions (length result))
             (1+ tests)
             (cdr remaining-tests)))))))

(define (create-suitbl-test-runner)
  (define state (make-atomic-box '()))
  (define last-run-summary (make-atomic-box #f))

  (define (update-atomic-alist-value! alist-atom key f)
    (atomic-box-update!
     alist-atom
     (lambda (alist)
       (let* ((value (or (assoc-ref alist key) #f))
              (new-value (f value)))
         (chain alist
                (alist-delete key _)
                (alist-cons key new-value _))))))

  (define (test-runner x)
    "Default test runner"
    (unless (member (assoc-ref x 'type) '(get-state get-log))
      (update-atomic-alist-value!
       state 'events
       (lambda (l)
         (cons x (or l '())))))

    (let ((msg-type (assoc-ref x 'type)))
      (case msg-type
        ((get-state)
         state)
        ((get-log)
         (reverse (or (assoc-ref (atomic-box-ref state) 'events) '())))

        ((load-test-suite)
         (let* ((description (assoc-ref x 'description))
                (test-reporter (test-reporter*))

                (test-suite-enter! (lambda ()
                                     (test-reporter
                                      `((type . test-suite-enter)
                                        (description . ,description)))))
                (test-suite-leave! (lambda ()
                                     (test-reporter
                                      `((type . test-suite-leave)
                                        (description . ,description)))))
                (try-load-suite
                 (lambda ()
                   (test-suite-enter!)
                   (let ((result
                          (with-exception-handler
                           (lambda (ex)
                             (cons 'exception ex))
                           (lambda ()
                             (cons
                              'value
                               ((assoc-ref x 'load-test-suite-thunk))))
                           #:unwind? #t)))
                     (test-suite-leave!)
                     result))))

           (match (try-load-suite)
             (('exception . ex)
              (raise-exception ex))
             (('value . val)
              val))))
        ((schedule-test)
         (let* ((description (assoc-ref x 'description))
                (test-thunk
                 (lambda ()
                   (let ((test-reporter (test-reporter*)))
                     (test-reporter
                      `((type . test-start)
                        (description . ,description)))
                     ((assoc-ref x 'test-thunk))
                     (test-reporter
                      `((type . test-end)
                        (description . ,description))))))
                (test-item
                 (cons test-thunk
                       `((description . ,description)
                         (test-path . ,(%test-path*))))))
           (update-atomic-alist-value!
            state 'tests
            (lambda (l)
              (cons test-item (or l '()))))
           ((test-reporter*)
            `((type . test-scheduled)
              (description . ,description)))))

        ((run-assert)
         (let ((assert-thunk (assoc-ref x 'assert-thunk))
               (assert-quoted-form (assoc-ref x 'assert-quoted-form)))
           (when (and (not (null? (%test-path*)))
                      (not (%test*)))
             (chain
              "Assert encountered inside test-suite, but outside of test"
              (make-exception-with-message _)
              (raise-exception _)))
           (default-run-assert assert-thunk #f assert-quoted-form)))

        ((run-scheduled-tests)
         (atomic-box-set!
          last-run-summary
          (chain
           (atomic-box-ref state)
           (assoc-ref _ 'tests)
           (or _ '())
           ;; (sort _ (lambda (x y) (rand-boolean)))
           ;; (for-each (lambda (t) ((car t))) _)
           (reverse _)
           (run-scheduled-tests _)))
         (atomic-box-ref last-run-summary))

        ((get-run-summary)
         (atomic-box-ref last-run-summary))

        (else
         (raise-exception
          (make-exception-with-message
           (format #f "no handler for message type ~a" msg-type)))))))
  test-runner)

(define (schedule-and-run-test-suits test-runner test-suits)
  (parameterize ((%current-test-runner* test-runner))
    ;; TODO: [Andrew Tropin, 2025-05-01] Call reset-runner-state
    (for-each (lambda (ts) (ts)) test-suits)
    (test-runner
     `((type . run-scheduled-tests)))
    ;; TODO: [Andrew Tropin, 2025-05-01] Call get-last-run-summary
    ))


(define get-test-runner* (make-parameter create-suitbl-test-runner))
(define %current-test-runner* (make-parameter #f))

(define (get-current-or-create-test-runner)
  "Tries to obtain current test runner and if there is no such present
creates one."
  (or
   (%current-test-runner*)
   ((get-test-runner*))))


(define %test-path* (make-parameter '()))
(define %test* (make-parameter #f))

(define-syntax is
  (lambda (x)
    (syntax-case x ()
      ((_ (pred args ...))
       (with-syntax ((form #'(pred args ...)))
         #'((get-current-or-create-test-runner)
            `((type . run-assert)
              (assert-thunk . ,(lambda () form))
              (assert-args-thunk . ,(lambda () (list args ...)))
              (assert-quoted-form .  form)))))
      ((_ form)
       #'((get-current-or-create-test-runner)
          `((type . run-assert)
            (assert-thunk . ,(lambda () form))
            (assert-quoted-form . form)))))))

(define-syntax test
  (lambda (x)
    "Test case represent a logical unit of testing, can include zero or
more asserts."
    (syntax-case x ()
      ((test case-description expression expressions ...)
       #'(let ((test-thunk
                (lambda ()
                  ;; TODO: [Andrew Tropin, 2025-04-11] Notify test case
                  ;; started (for cases with zero asserts)
                  (when (%test*)
                    (raise-exception
                     (make-exception-with-message "Test Cases can't be nested")))
                  (parameterize ((%current-test-runner*
                                  (get-current-or-create-test-runner))
                                 (%test* case-description))
                    expression
                    expressions ...))))
           (let ((test-runner (get-current-or-create-test-runner)))
             (test-runner
              `((type . schedule-test)
                (test-thunk . ,test-thunk)
                (description . ,case-description)
                (test-body . (expression expressions ...))))
             (when (null? (%test-path*))
               (test-runner `((type . run-scheduled-tests)))))))
      ((_ rest ...)
       #'(syntax-error "Wrong usage of test")))))

(define-syntax test-suite
  (lambda (x)
    "Test suite is simple unit of testing, it can be executed in parallel,
allows to group tests and other test suits."
    (syntax-case x ()
      ((_ suite-description expression ...)
       #'(let* ((load-test-suite-thunk
                 (lambda ()
                   (when (%test*)
                     (raise-exception
                      (make-exception-with-message
                       "Test Suite can't be nested into Test Macro")))

                   ;; TODO: [Andrew Tropin, 2025-05-05] The test
                   ;; runner definitely exists at this point of time,
                   ;; because somebody already called load-test-suite
                   ;; method of a test runner, and that's why we got
                   ;; here.  Clean up this parameterization and think
                   ;; who need to call run-scheduled-tests (probably
                   ;; not this function).
                   (parameterize ((%current-test-runner*
                                   (get-current-or-create-test-runner))
                                  (%test-path*
                                   (cons suite-description (%test-path*))))
                     expression ...)))
                (test-suite-thunk
                 (lambda ()
                   (let ((test-runner (get-current-or-create-test-runner)))
                     (test-runner
                      `((type . load-test-suite)
                        (load-test-suite-thunk . ,load-test-suite-thunk)
                        (description . ,suite-description)))
                     ;; TODO: [Andrew Tropin, 2025-05-05] Can be done
                     ;; on a test runner side, right?
                     (when (null? (%test-path*))
                       (test-runner `((type . run-scheduled-tests))))))))
           (set-procedure-properties!
            test-suite-thunk
            `((name . test-suite)
              (documentation . ,suite-description)
              (srfi-264-test-suite? . #t)))
           test-suite-thunk)))))

(define-syntax define-test-suite
  (syntax-rules ()
    ((_ test-suite-name expression ...)
     (define test-suite-name
       (test-suite (symbol->string 'test-suite-name) expression ...)))))

(define-syntax reset-test-environment
  (lambda (stx)
    (syntax-case stx ()
      ((_ get-test-runner body body* ...)
       #'(parameterize ((%current-test-runner* (get-test-runner))
                        (%test-path* '())
                        (%test* #f))
           body body* ...)))))

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
