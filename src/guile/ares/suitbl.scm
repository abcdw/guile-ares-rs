;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 exceptions)
  #:use-module (ares atomic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-197)
  #:export (create-suitbl-test-runner
            schedule-and-run-test-suits
            reset-test-environment
            get-current-or-create-test-runner
            test-reporter-output-port*
            define-test-suite
            test-suite test-case is
            throws-exception?))

;; TODO: [Andrew Tropin, 2025-02-19] Look at

#|

SUITBL is Scheme Universal Interactive Testing Base Library

Test cases can be combined by another define-test:
(define-test addition
  (is (= 4 (+ 2 2)))
  (is (= 7 (+ 3 4))))

(define-test subtraction
  (is (= 1 (- 4 3)))
  (is (= 3 (- 7 4))))

(define-test arithmetic
  (addition)
  (subtraction))

|#


;;;
;;; Reporters
;;;

(define test-reporter-output-port* (make-parameter (current-output-port)))

(define (suitbl-base-reporter message)
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
             (string-repeat "-" (length (%test-path*))))
     (format (test-reporter-output-port*)
             "> suite entered: ~a\n" (assoc-ref message 'description)))
    ((test-suite-leave)
     (format (test-reporter-output-port*)
             (string-repeat "-" (length (%test-path*))))
     (format (test-reporter-output-port*)
             "> suite left: ~a\n" (assoc-ref message 'description)))

    ((test-case-start)
     (format (test-reporter-output-port*)
             "\n┌Test case started: ~a\n" (assoc-ref message 'description)))
    ((test-case-end)
     (format (test-reporter-output-port*)
             "└Test case ended: ~a\n" (assoc-ref message 'description)))

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

(define test-reporter* (make-parameter suitbl-base-reporter))


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
execute or asserts, test-cases and test-suites can instantiate some
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

(define %test-case-events* (make-parameter #f))

(define (default-run-assert form-thunk args-thunk quoted-form)
  (with-exception-handler
   (lambda (ex)
     (when (%test-case*)
       (atomic-box-update!
        (%test-case-events*)
        (lambda (value)
          (cons 'error value))))
     (suitbl-base-reporter
      `((type . assert-error)
        (quoted-form . ,quoted-form)
        (error . ,ex))))
   (lambda ()
     ;; TODO: [Andrew Tropin, 2024-12-23] Write down evaluation time
     ;; TODO: [Andrew Tropin, 2024-12-23] Report start before evaling the form
     (let* ((result (form-thunk)))
       (when (%test-case*)
         (atomic-box-update!
          (%test-case-events*)
          (lambda (value)
            (cons (if result 'pass 'fail) value))))
       (suitbl-base-reporter
        `((type . ,(if result 'assert-pass 'assert-fail))
          (result . ,result)
          (quoted-form . ,quoted-form)))
       result))
   #:unwind? #t))


(define (run-test-case test-case-thunk)
  (parameterize ((%test-case-events* (make-atomic-box '())))
    ;; TODO: [Andrew Tropin, 2025-04-24] Handle exceptions that can
    ;; happen inside test case, but outside of assert
    (test-case-thunk)
    (atomic-box-ref (%test-case-events*))))

(define (run-scheduled-test-cases test-cases)
  ;; pass, fail, skip(?), error
  (let loop ((errors 0)
             (failures 0)
             (assertions 0)
             (tests 0)
             (remaining-test-cases test-cases))
    (if (null? remaining-test-cases)
        `((errors . ,errors)
          (failures . ,failures)
          (assertions . ,assertions)
          (tests . ,tests))
        (begin
          (let* ((result (run-test-case (caar remaining-test-cases)))
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
             (cdr remaining-test-cases)))))))

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

        ((test-suite-enter test-suite-leave test-case-start test-case-end)
         (suitbl-base-reporter x))

        ;; TODO: [Andrew Tropin, 2025-04-22] Rename it to schedule-test-case-run
        ((schedule-test-case)
         (let* ((test-case-thunk
                 (lambda ()
                   ((assoc-ref x 'test-case-thunk))))
                (description (assoc-ref x 'description))
                (test-case-item
                 (cons test-case-thunk
                       `((description . ,description)
                         (test-path . ,(%test-path*))))))
           (update-atomic-alist-value!
            state 'test-cases
            (lambda (l)
              (cons test-case-item (or l '()))))
           'test-case-scheduled))

        ((run-assert)
         (let ((assert-thunk (assoc-ref x 'assert-thunk))
               (assert-quoted-form (assoc-ref x 'assert-quoted-form)))
           (when (and (not (null? (%test-path*)))
                      (not (%test-case*)))
             (chain
              "Assert encountered inside test-suite, but outside of test-case"
              (make-exception-with-message _)
              (raise-exception _)))
           (default-run-assert assert-thunk #f assert-quoted-form)))

        ((run-scheduled-test-cases)
         (atomic-box-set!
          last-run-summary
          (chain
           (atomic-box-ref state)
           (assoc-ref _ 'test-cases)
           (or _ '())
           ;; (sort _ (lambda (x y) (rand-boolean)))
           ;; (for-each (lambda (t) ((car t))) _)
           (reverse _)
           (run-scheduled-test-cases _)))
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
     `((type . run-scheduled-test-cases)))
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
(define %test-case* (make-parameter #f))

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

(define-syntax test-case
  (lambda (x)
    "Test case represent a logical unit of testing, can include zero or
more asserts."
    (syntax-case x ()
      ((test-case case-description expression expressions ...)
       #'(let ((test-case-thunk
                (lambda ()
                  ;; TODO: [Andrew Tropin, 2025-04-11] Notify test case
                  ;; started (for cases with zero asserts)
                  (when (%test-case*)
                    (raise-exception
                     (make-exception-with-message "Test Cases can't be nested")))
                  (let ((test-runner (get-current-or-create-test-runner)))
                    (parameterize ((%current-test-runner* test-runner)
                                   (%test-case* case-description))
                      (test-runner
                       `((type . test-case-start)
                         (description . ,case-description)))
                      expression
                      expressions ...
                      (test-runner
                       `((type . test-case-end)
                         (description . ,case-description))))))))
           (let ((test-runner (get-current-or-create-test-runner)))
             (test-runner
              `((type . schedule-test-case)
                (test-case-thunk . ,test-case-thunk)
                (description . ,case-description)
                (test-case-body . (expression expressions ...))))
             (when (null? (%test-path*))
               (test-runner `((type . run-scheduled-test-cases)))))))
      ((_ rest ...)
       #'(syntax-error "Wrong usage of test-case")))))

(define-syntax test-suite
  (lambda (x)
    "Test suite is simple unit of testing, it can be executed in parallel,
allows to group test cases, can include other test suits."
    (syntax-case x ()
      ((_ suite-description expression ...)
       #'(let ((test-suite-thunk
                (lambda ()
                  (when (%test-case*)
                    (raise-exception
                     (make-exception-with-message
                      "Test Suite can't be nested into Test Case")))
                  (let ((test-runner (get-current-or-create-test-runner)))
                    (parameterize ((%current-test-runner* test-runner)
                                   (%test-path*
                                    (cons suite-description (%test-path*))))
                      (test-runner
                       `((type . test-suite-enter)
                         (description . ,suite-description)))
                      expression ...
                      (test-runner
                       `((type . test-suite-leave)
                         (description . ,suite-description))))
                    (when (null? (%test-path*))
                      (test-runner `((type . run-scheduled-test-cases))))))))
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
                        (%test-case* #f))
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
