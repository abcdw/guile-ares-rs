;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl)
  #:use-module ((ice-9 atomic)
                #:select (make-atomic-box atomic-box-ref atomic-box-set!))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((ice-9 exceptions) #:select (make-exception-with-message))
  #:use-module ((ice-9 ftw) #:select (nftw))
  #:use-module ((ice-9 regex) #:select (string-match))
  #:use-module ((ares atomic) #:select (atomic-box-update!))
  #:use-module ((srfi srfi-1)
                #:select (last drop-right any fold alist-delete alist-cons))
  #:use-module ((srfi srfi-197) #:select (chain))

  ;; TODO: [Andrew Tropin, 2025-05-15] reset-test-environment ->
  ;; with-clean-test-environment?/test-environment-clean
  #:export (reset-test-environment
            test-suite test is

            test-reporter-output-port*
            ;; TODO: [Andrew Tropin, 2025-05-15] Add support for
            ;; multiple composable test reporters? or at least make
            ;; test-reporters more composable, so we can combine them
            ;; in "megareporter" and use it as a test-reporter*
            test-reporter*

            test-reporter-silent
            test-reporter-base
            test-reporter-dots

            ;; TODO: [Andrew Tropin, 2025-05-15] Implement composable
            ;; test-reporter-print-failures-and-errors, which will be
            ;; executed at the end and provide detailed info of
            ;; locations with failed tests

            ;; TODO: [Andrew Tropin, 2025-05-15] Create separate
            ;; test-reporter-asserrt-minimal (showing only ✗ or ✓) and
            ;; test-reporter-assert-simple (like current base), which
            ;; can be used in base reporter.

            test-runner-create-suitbl
            test-runner-get-current-or-create
            test-runner-run-test-suites

            ;; TODO: [Andrew Tropin, 2025-05-15] Remove it?, because it
            ;; introduces ambiguity and doesn't have a private
            ;; counterpart
            define-test-suite

            throws-exception?))


#|

SUITBL is Scheme Universal Interactive Testing Base Library

The primary tool of the test library is assert macro @code{is}.  You
can use it on it's own and evaluate it, test runner will take care of
executing the body of assert and calling reporter to report result.

Asserts can be grouped with test macro to make a simple unit of
testing.  Test can also be evaluated on it's own, test runner knows
what to do with it.

Tests can be grouped into test suites.  Test suites can be nested in
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
nested tests and test suites add it into test runner, later those
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

  (define (actual message)
    (let ((quoted-form (assoc-ref message 'assert/quoted-form))
          (arguments-thunk (assoc-ref message 'assert/arguments-thunk)))
      (if (= 3 (length quoted-form))
          (match (arguments-thunk)
            ((first second)
             (format #f "~a and ~a are not ~a" first second (car quoted-form))))
          (assoc-ref message 'assert/result))))

  (define msg-type (assoc-ref message 'type))
  (case msg-type
    ((test-suite-enter)
     (format (test-reporter-output-port*)
             (string-append
              (string-repeat "|" (length (assoc-ref message 'test-path)))
              "┌"))
     (format (test-reporter-output-port*)
             "> ~a\n" (assoc-ref message 'description)))
    ((test-suite-leave)
     (format (test-reporter-output-port*)
             (string-append
              (string-repeat "|" (length (assoc-ref message 'test-path)))
              "└"))
     (format (test-reporter-output-port*)
             "> ~a\n" (assoc-ref message 'description)))

    ((test-scheduled)
     (format (test-reporter-output-port*)
             (string-repeat "|" (length (assoc-ref message 'test-path))))
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
             (assoc-ref message 'assert/quoted-form)))

    ((assert-fail)
     (format (test-reporter-output-port*) "✗ Expected: ~s\n   Actual:   ~a\n"
             (assoc-ref message 'assert/quoted-form) (actual message)))

    ((assert-error)
     (format (test-reporter-output-port*) "✗ ~s produced error:\n   ~s\n"
             (assoc-ref message 'assert/quoted-form)
             (assoc-ref message 'assert/error)))
    ((test-suite-start test-suite-end test-skip)
     'do-nothing)

    (else
     (raise-exception
      (make-exception-with-message
       (format #f "no reporting implemented for message type ~a" msg-type))))))

(define (test-reporter-silent message)
  #f)

(define (test-reporter-dots message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type
    ((test-suite-start)
     (format (test-reporter-output-port*) "["))
    ((test-suite-end)
     (format (test-reporter-output-port*) "]"))

    ((test-start)
     (format (test-reporter-output-port*) "("))
    ((test-end)
     (format (test-reporter-output-port*) ")"))
    ((test-skip)
     (format (test-reporter-output-port*) "(S)"))

    ((assert-pass)
     (format (test-reporter-output-port*) "."))
    ((assert-fail)
     (format (test-reporter-output-port*) "F"))
    ((assert-error)
     (format (test-reporter-output-port*) "E"))

    ((test-scheduled test-suite-enter test-suite-leave)
     'do-nothing)

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
        (assert/error . ,ex))))
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
          (assert/result . ,result)
          (assert/arguments-thunk . ,args-thunk)
          (assert/quoted-form . ,quoted-form)))
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

(define (test? x)
  (and (procedure? x)
       (procedure-property x 'suitbl-test?)))

(define (test-suite? x)
  (and (procedure? x)
       (procedure-property x 'suitbl-test-suite?)))

(define (run-scheduled-test test)
  (let* ((result (run-test test))
         (error? (any (lambda (x) (eq? x 'error)) result))
         (fail? (any (lambda (x) (eq? x 'fail)) result)))
    `((errors . ,(if error? 1 0))
      (failures . ,(if (and fail? (not error?)) 1 0))
      (assertions . ,(length result))
      (tests . 1))))

(define (merge-summaries s1 s2)
  (map
   (lambda (v)
     (match v
       ((key . value)
        (cons key (+ (assoc-ref s2 key) value)))))
   s1))

(define (run-scheduled-suite suite)
  ;; TODO: [Andrew Tropin, 2025-05-08] Add reporting of suite start/end
  (let ((result #f))
    ((test-reporter*)
     `((type . test-suite-start)
       (description . ,(car suite))))
    (set! result
          (let loop ((summary `((errors . 0)
                                (failures . 0)
                                (assertions . 0)
                                (tests . 0)))
                     (remaining-items (cdr suite)))
            (if (null? remaining-items)
                summary
                (let ((item (car remaining-items)))
                  (loop
                   (merge-summaries
                    summary
                    ((if (test? item) run-scheduled-test run-scheduled-suite)
                     item))
                   (cdr remaining-items))))))
    ((test-reporter*)
     `((type . test-suite-end)
       (description . ,(car suite))))
    result))

;; A temporary hack to prevent double execution
(define %external-executor* (make-parameter #f))

(define (test-runner-create-suitbl)
  (define state (make-atomic-box '()))
  (define last-run-summary (make-atomic-box #f))
  (define %current-test-suite-items* (make-parameter #f))

  (define (update-atomic-alist-value! alist-atom key f)
    (atomic-box-update!
     alist-atom
     (lambda (alist)
       (let* ((value (or (assoc-ref alist key) #f))
              (new-value (f value)))
         (chain alist
                (alist-delete key _)
                (alist-cons key new-value _))))))

  (define (print-test-suite suite)
    (define (prettify-list l)
      (map
       (lambda (i)
         (cond
          ((test? i) (string-append "test: " (procedure-documentation i)))
          ((string? i) (string-append "suite: " i))
          ((list? i) (prettify-list i))
          (else i)))
       l))
    (format #t "~y" (prettify-list suite)))

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
                                        (test-path . ,(%test-path*))
                                        (description . ,description)))))
                (test-suite-leave! (lambda ()
                                     (test-reporter
                                      `((type . test-suite-leave)
                                        (test-path . ,(%test-path*))
                                        (description . ,description)))))
                (try-load-suite
                 (lambda ()
                   (test-suite-enter!)
                   (let ((result
                          (with-exception-handler
                           (lambda (ex)
                             (cons 'exception ex))
                           (lambda ()
                             (when (%test*)
                               (raise-exception
                                (make-exception-with-message
                                 "Test Suite can't be nested into Test Macro")))
                             ;; TODO: [Andrew Tropin, 2025-05-08]
                             ;; Change the condition of top-level
                             ;; suite.  Or wrap list of test-suites
                             ;; into one more.
                             (parameterize ((%current-test-suite-items*
                                             (make-atomic-box '())))
                               ((assoc-ref x 'load-test-suite-thunk))
                               (chain (%current-test-suite-items*)
                                      (atomic-box-ref _)
                                      (reverse _)
                                      (cons description _)
                                      (cons 'value _))))
                           #:unwind? #t)))
                     (test-suite-leave!)
                     result))))

           (match (try-load-suite)
             (('exception . ex)
              (raise-exception ex))
             (('value . val)
              (let ((suite-items (%current-test-suite-items*)))
                (if suite-items
                    (atomic-box-update!
                     suite-items
                     (lambda (items) (cons val items)))

                    (update-atomic-alist-value! state 'suite (lambda (l) val))))
              val))
           ;; test-runner-run-test-suites sets %external-executor*
           ;; and also calls run-scheduled-tests, so to prevent double
           ;; execution of scheduled test suites we add this condition.
           (when (and (null? (%test-path*)) (not (%external-executor*)))
             ((test-runner-get-current-or-create)
              `((type . run-scheduled-tests))))))

        ((schedule-test)
         (let* ((original-test-thunk (assoc-ref x 'test-thunk))
                (description (procedure-documentation original-test-thunk))
                (new-test-thunk
                 (lambda ()
                   (when (%test*)
                     (chain "Test Macros can't be nested"
                            (make-exception-with-message _)
                            (raise-exception _)))
                   (let ((test-reporter (test-reporter*)))
                     (test-reporter
                      `((type . test-start)
                        (description . ,description)))
                     (original-test-thunk)
                     (test-reporter
                      `((type . test-end)
                        (description . ,description))))))
                (test-item new-test-thunk))

           (set-procedure-properties!
            new-test-thunk
            (procedure-properties original-test-thunk))
           (let ((suite-items (%current-test-suite-items*)))
             (if suite-items
                 (atomic-box-update!
                  suite-items
                  (lambda (items) (cons test-item items)))
                 (update-atomic-alist-value!
                  state 'suite
                  ;; TODO: [Andrew Tropin, 2025-05-08] Remove unnamed suite wrap
                  (lambda (l) (list "unnamed suite" test-item)))))

           (update-atomic-alist-value!
            state 'tests
            (lambda (l)
              (cons test-item (or l '()))))
           ((test-reporter*)
            `((type . test-scheduled)
              (test-path . ,(%test-path*))
              (description . ,description)))
           (when (null? (%test-path*))
             ((%current-test-runner*)
              `((type . run-scheduled-tests))))))

        ((run-assert)
         (let ((assert-thunk (assoc-ref x 'assert/thunk))
               (assert-arguments-thunk (assoc-ref x 'assert/arguments-thunk))
               (assert-quoted-form (assoc-ref x 'assert/quoted-form)))
           (when (and (not (null? (%test-path*)))
                      (not (%test*)))
             (chain
              "Assert encountered inside test-suite, but outside of test"
              (make-exception-with-message _)
              (raise-exception _)))
           (default-run-assert
             assert-thunk assert-arguments-thunk assert-quoted-form)))

        ((run-scheduled-tests)
         ;; (print-test-suite (assoc-ref (atomic-box-ref state) 'suite))
         (atomic-box-set!
          last-run-summary
          (chain
           (atomic-box-ref state)
           (assoc-ref _ 'suite)
           (run-scheduled-suite _)))

         (atomic-box-ref last-run-summary))

        ((get-run-summary)
         (atomic-box-ref last-run-summary))

        (else
         (raise-exception
          (make-exception-with-message
           (format #f "no handler for message type ~a" msg-type)))))))
  test-runner)

(define (test-runner-run-test-suites test-runner test-suites)
  (parameterize ((%current-test-runner* test-runner)
                 (%external-executor* #t))
    ;; TODO: [Andrew Tropin, 2025-05-01] Call reset-runner-state
    (for-each (lambda (ts) (ts)) test-suites)
    ;; TODO: [Andrew Tropin, 2025-05-08] Prevent execution of test
    ;; suites, only schedule them

    (test-runner
     `((type . run-scheduled-tests)))
    ;; TODO: [Andrew Tropin, 2025-05-01] Call get-last-run-summary
    ))


(define get-test-runner* (make-parameter test-runner-create-suitbl))
(define %current-test-runner* (make-parameter #f))

(define (test-runner-get-current-or-create)
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
         #'((test-runner-get-current-or-create)
            `((type . run-assert)
              (assert/thunk . ,(lambda () form))
              (assert/arguments-thunk . ,(lambda () (list args ...)))
              (assert/quoted-form .  form)))))
      ((_ form)
       #'((test-runner-get-current-or-create)
          `((type . run-assert)
            (assert/thunk . ,(lambda () form))
            (assert/quoted-form . form)))))))

(define (alist-merge l1 l2)
  (append l1 l2))

(define-syntax test
  (lambda (x)
    "Test case represent a logical unit of testing, can include zero or
more asserts."
    (syntax-case x ()
      ((test case-description #:metadata metadata expression expressions ...)
       #'(let ((test-thunk
                (lambda ()
                  ;; TODO: [Andrew Tropin, 2025-05-15] Move this logic
                  ;; to test-runner side because right now to
                  ;; implement a usable test runner we need to access
                  ;; those variables and thus export them as public
                  ;; API.  We can avoid all of those problems by
                  ;; offloading this to test runner implementers.
                  ;; They need to keep track of the test suite
                  ;; hierarchy and all that stuff anyway.

                  ;; The only problem I see here is that
                  ;; test-environment-reset macro won't be able to
                  ;; clean %test* and %test-path*, so it must be
                  ;; implemented on test runner side as well.  Which
                  ;; is a bit sad.  However, the new test runner will
                  ;; have new parameters, which will be empty.  So the
                  ;; resetting test-runner is enough.  Also, we can
                  ;; have (with-clean-test-env new-runner thunk),
                  ;; which will run thunk in clean test env.

                  (parameterize ((%current-test-runner*
                                  (test-runner-get-current-or-create))
                                 (%test* case-description))
                    expression
                    expressions ...))))
           (set-procedure-properties!
            test-thunk
            (alist-merge
             metadata
             `((name . test)
               (documentation . ,case-description)
               (suitbl-test? . #t))))
           (parameterize ((%current-test-runner*
                           (test-runner-get-current-or-create)))
             ((%current-test-runner*)
              `((type . schedule-test)
                (test-thunk . ,test-thunk)
                (description . ,case-description)
                (test-body . (expression expressions ...)))))))
      ((test case-description expression expressions ...)
       #'(test case-description #:metadata '() expression expressions ...))
      ((_ rest ...)
       #'(syntax-error "Wrong usage of test")))))

;; TODO: [Andrew Tropin, 2025-05-15] Add metedata to test-suite
(define-syntax test-suite
  (lambda (x)
    "Test suite is simple unit of testing, it can be executed in parallel,
allows to group tests and other test suites."
    (syntax-case x ()
      ((_ suite-description expression expressions ...)
       #'(let* ((load-test-suite-thunk
                 (lambda ()
                   ;; TODO: [Andrew Tropin, 2025-05-05] The test
                   ;; runner definitely exists at this point of time,
                   ;; because somebody already called load-test-suite
                   ;; method of a test runner, and that's why we got
                   ;; here.  Clean up this parameterization and think
                   ;; who need to call run-scheduled-tests (probably
                   ;; not this function).
                   (parameterize ((%current-test-runner*
                                   (test-runner-get-current-or-create))
                                  (%test-path*
                                   (cons suite-description (%test-path*))))
                     expression
                     expressions ...)))
                (test-suite-thunk
                 ;; Wrapping into identity to prevent setting procedure-name
                 (identity
                  (lambda ()
                    (parameterize ((%current-test-runner*
                                    (test-runner-get-current-or-create)))
                      ((%current-test-runner*)
                       `((type . load-test-suite)
                         (load-test-suite-thunk . ,load-test-suite-thunk)
                         (description . ,suite-description))))))))
           (set-procedure-properties!
            test-suite-thunk
            `((documentation . ,suite-description)
              (suitbl-test-suite? . #t)))
           test-suite-thunk)))))

(define-syntax define-test-suite
  (syntax-rules ()
    ((_ test-suite-name expression ...)
     (begin
       (define-public test-suite-name
         (test-suite (symbol->string 'test-suite-name) expression ...))
       (set-procedure-property! test-suite-name 'name 'test-suite-name)))))

(define-syntax reset-test-environment
  (lambda (stx)
    (syntax-case stx ()
      ((_ get-test-runner body body* ...)
       #'(parameterize ((%current-test-runner* (get-test-runner))
                        (%external-executor* #f)
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


;;;
;;; Test Suite Exploration API
;;;

(define (get-test-module name)
  ;; TODO: Handle the case, when test module doesn't exist.
  "Return a test module related to the specified.  Usually it's a module with
-test prefix.  Return current module if it already contains -test prefix."
  (let* ((m-name name)
         (m-tail (last m-name))
         (test-m-tail
          (if (string-suffix? "-test" (symbol->string m-tail))
              m-tail
              (symbol-append m-tail '-test))))
    (resolve-module
     (append
      (drop-right m-name 1)
      (list test-m-tail)))))

;; (get-test-module '(ares suitbl))

(define (get-module-test-suites module)
  (filter
   identity
   (module-map (lambda (k v)
                 (and (variable-bound? v)
                      (test-suite? (variable-ref v))
                      (variable-ref v)))
               module)))

(define (get-module-public-test-suites module)
  (get-module-test-suites (module-public-interface module)))

(define private-samples-tests
  (test-suite "sample private test suite"
    (test "sample test"
      (is "true assert"))))
;; (private-samples-tests)

(define* (load-test-modules-thunk
          #:key
          (test-file-pattern ".*-test(\\.scm|\\.ss)")
          (load-file (lambda (p rp)
                       (format #t "loading test module: ~a\n" p)
                       (primitive-load-path rp))))
  "Return a thunk, which loads all the modules matching TEST-FILE-PATTERN
using LOAD-FILE procedure, which accepts path and relative to %load-path path."
  (lambda ()
    (for-each
     (lambda (path)
       (nftw
        path
        (lambda (file-path _ flags _1 _2)
          (when (eq? flags 'regular)
            (define relative-path
              (string-drop file-path (1+ (string-length path))))
            (when (string-match test-file-pattern file-path)
              (load-file file-path relative-path)))
          #t)))
     %load-path)))

(define* (get-all-test-modules
          #:key
          (load-project-test-modules? #t)
          (load-all-test-modules (load-test-modules-thunk)))
  (when load-project-test-modules?
    (load-all-test-modules))
  ;; TODO: [Andrew Tropin, 2025-05-09] Use module-name regex instead
  ;; of suffix match?
  (filter (lambda (m)
            (string-suffix? "-test" (symbol->string (last (module-name m)))))
          ((@ (ares reflection modules) all-modules))))

;; ((@ (ice-9 pretty-print) pretty-print)
;;  (map
;;   (lambda (m)
;;     (cons
;;      (module-name m)
;;      (get-module-public-test-suites m))) (get-all-test-modules)))

;; (define-test-suite public-suite
;;   'hey)

;; (variable-set!)
;; (get-module-test-suites (module-public-interface (current-module)))
