;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl)
  #:use-module (ice-9 atomic)
  #:use-module (ares atomic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-197)
  #:export (define-test is))

;; TODO: [Andrew Tropin, 2025-02-19] Look at

;; https://gerbil.scheme.org/reference/dev/test.html
;; https://gerbil.scheme.org/reference/std/test.html#test-suite
;; check macro and various convinience wrappers

;; https://srfi.schemers.org/srfi-78/srfi-78.html
;; https://cljdoc.org/d/lambdaisland/kaocha/1.91.1392/doc/5-running-kaocha-from-the-repl

;; http://testanything.org/tap-version-14-specification.html
;; test output specification

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


Questions:
1.
How to backlink test to function, so you can see all the tests related
to the function?

2.
Test tags, which can be used to produce test suits (subset of tests):
unit, integration, acceptance, backend, frontend
https://github.com/testmoapp/junitxml

3.
Watch? for changed tests/implementations?

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
load-tests* variable, which controls macro expansion logic, setting it
to #f will make all test defining functions to produce empty results.

|#




;;; Test runner

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


;; TODO: [Andrew Tropin, 2025-04-15] Make it private
(define test-output-port* (make-parameter (current-output-port)))

(define (default-report type params)
  "Default report implementation"
  (case type
    ((pass) (format (test-output-port*) "✓ ~s\n"
                    (assoc-ref params 'expected)))
    ((fail) (format (test-output-port*) "✗~%  Expected: ~s~%  ~a: ~s\n"
                    (assoc-ref params 'expected)
                    (if (assoc-ref params 'error) "Error" "Actual")
                    (or (assoc-ref params 'error)
                        (assoc-ref params 'actual))))
    (else (throw 'no-such-handler))))

(define-syntax simple-profile
  (lambda (stx)
    (syntax-case stx ()
      ((_ expressions ...)
       #'(let ((start-time (get-internal-real-time))
               (return-value expressions ...))
           (format #t "run time: ~f\n"
                   (exact->inexact
                    (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
           return-value)))))

(define (default-run-assert form-thunk args-thunk quoted-form)
  (simple-profile
   (with-exception-handler
    (lambda (ex)
      (default-report 'fail
        `((expected . ,quoted-form)
          (error . ,ex))))
    (lambda ()
      ;; TODO: [Andrew Tropin, 2024-12-23] Write down evaluation time
      ;; TODO: [Andrew Tropin, 2024-12-23] Report start before evaling the form
      (let* ((result (form-thunk)))
        (default-report (if result 'pass 'fail)
          `((expected . ,quoted-form)
            (actual . (not ,quoted-form))))
        result))
    #:unwind? #t)))


;; TODO: [Andrew Tropin, 2025-04-22] Collect all test cases in a list
;; and add info about test-path and possible other things (like fixtures?)

(define (default-get-test-runner)
  (define state (make-atomic-box '()))
  (define (update-atomic-alist-value! alist-atom key f)
    (atomic-box-update!
     alist-atom
     (lambda (alist)
       (let* ((value (or (assoc-ref alist key) #f))
              (new-value (f value)))
         (chain alist
                (alist-delete key _)
                (alist-cons key new-value _))))))

  (define (string-repeat s n)
    "Returns string S repeated N times."
    (fold
     (lambda (_ str)
       (string-append str s))
     ""
     (iota n)))

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

        ((test-suite-start)
         (newline)
         (format #t (string-repeat "-" (length (%test-path*))))
         (format #t "> suite started: ~a\n" (assoc-ref x 'description)))
        ((test-suite-end)
         (format #t (string-repeat "-" (length (%test-path*))))
         (format #t "> suite ended: ~a\n" (assoc-ref x 'description)))

        ((test-case-start)
         (format #t "\n┌Test case started: ~a\n" (assoc-ref x 'description)))
        ((test-case-end)
         (format #t "└Test case ended: ~a\n" (assoc-ref x 'description)))

        ((run-test-case)
         (let ((test-case-thunk (assoc-ref x 'test-case-thunk)))
           (simple-profile
            (test-case-thunk))))

        ((run-assert)
         (let ((assert-thunk (assoc-ref x 'assert-thunk))
               (assert-quoted-form (assoc-ref x 'assert-quoted-form)))

           (if (%test-case*)
               (default-run-assert assert-thunk #f assert-quoted-form)
               (test-case
                "anonymous"
                (default-run-assert assert-thunk #f assert-quoted-form)))))

        (else
         (raise-exception
          (make-exception-with-message
           (format #f "no handler for message type ~a" msg-type)))))))
  test-runner)

(define (get-silent-test-runner)
  (define (test-runner x)
    "Default test runner"
    (let ((msg-type (assoc-ref x 'type)))
      (case msg-type
        ((run-test-case)
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

(define get-test-runner* (make-parameter default-get-test-runner))
(define %current-test-runner* (make-parameter #f))

(define (get-current-test-runner)
  (or
   (%current-test-runner*)
   ((get-test-runner*))))

(define tr ((get-test-runner*)))

;; (tr `((type . run-assert)
;;       (assert-thunk . ,(lambda () (format #t "I'm dummy assert\n")))
;;       (assert-quoted-form . (format #t "I'm dummy assert\n"))))
;; (tr '((type . get-log)))

;; (parameterize ((%current-test-runner* tr))
;;   (is #t))

;; (tr '((type . add-event)
;;       (event . "something happened again")))


(define %test-path* (make-parameter '()))
(define %test-case* (make-parameter #f))

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
                  (parameterize ((%test-case* case-description))
                    (let ((test-runner (get-current-test-runner)))
                      (test-runner
                       `((type . test-case-start)
                         (description . ,case-description)))
                      expression
                      expressions ...
                      (test-runner
                       `((type . test-case-end)
                         (description . ,case-description))))))))
           ((get-current-test-runner)
            `((type . run-test-case)
              (test-case-thunk . ,test-case-thunk)
              (test-case-body . (expression expressions ...))))))
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
                  (let ((tr (get-current-test-runner)))
                    (parameterize ((%test-path*
                                    (cons suite-description (%test-path*))))
                      (tr
                       `((type . test-suite-start)
                         (description . ,suite-description)))
                      expression ...
                      (tr
                       `((type . test-suite-end)
                         (description . ,suite-description))))))))
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
      ((_ get-test-runner expression ...)
       #'(parameterize ((%current-test-runner* (get-test-runner))
                        (%test-path* '())
                        (%test-case* #f))
           expression ...)))))



(define-syntax is
  (lambda (x)
    (syntax-case x ()
      ((_ (pred args ...))
       (with-syntax ((form #'(pred args ...)))
         #'((get-current-test-runner)
            `((type . run-assert)
              (assert-thunk . ,(lambda () form))
              (assert-args-thunk . ,(lambda () (list args ...)))
              (assert-quoted-form .  form)))))
      ((_ form)
       #'((get-current-test-runner)
          `((type . run-assert)
            (assert-thunk . ,(lambda () form))
            (assert-quoted-form . form)))))))

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




(define-test-suite different-is-usages
  (is #t)
  (define a 123)
  (is a)

  ;; TODO: [Andrew Tropin, 2025-04-08] Produce more sane error message
  ;; for cases with atomic or identifier expressions.
  (is #f)
  (is (lset= = '(1 2 2 3) '(2 3 4 5)))
  ;; (is (begin 'value1 'value2))
  ;; (is (= (throw 'hi) 7))

  (is (= 4 7))
  (is (= 4 (+ 2 2))))


(use-modules (ice-9 exceptions))

(define-test-suite nested-test-suites-and-test-cases
  ;; Nested testsuits requires double parentesis to be immediately
  ;; called on evaluation
  (is (throws-exception? (+ b 1 2) programming-error?))

  (test-case
   "nested test cases are forbidden"
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

  ((test-suite
    "hey hey there"
    (test-case
     "very true test case"
     (is #t)
     (is "very true"))
    ((test-suite
      "Hello there"
      (is (= 4 (+ 2 2)))))))

  (is (= 7 (+ 3 4))))

;; TODO: [Andrew Tropin, 2025-04-17] Make conditional expansion of is
;; and test-case, if they expanded+evaluated on their own they
;; immediately executed, however if they expanded inside context of
;; other macro (test-suite, define), they return a thunk.  It can be
;; useful to make test-case excutable in place, however still work
;; with define.  Sounds hacky and very implicit, so it's just an idea
;; to think about, not the call to action.

(define-test-suite addition
  (test-case
   "simple addition of small numbers"
   (is (= 4 (+ 2 2)))
   (is (= 7 (+ 3 4))))

  (test-case
   "addition of big numbers"
   (is (= 40000000000000000000000000
          (+ 20000000000000000000000000
             20000000000000000000000000)))))

(define-test-suite subtraction
  (is (= 2 (- 4 3)))
  (is (= 3 (- 7 4))))

(define-test-suite exception
  (is (= 3 (throw 'hi))))

(define-test-suite long-running-asserts
  (is (sleep 1)))

(define-test-suite all-tests
  (different-is-usages)
  (addition)
  (subtraction)
  ;; (exception)
  (nested-test-suites-and-test-cases)
  (long-running-asserts))

;; (all-tests)

;; TODO: [Andrew Tropin, 2025-04-11] Specify test timeouts to 10 by
;; default, so the test evaluation never hangs.

;; TODO: [Andrew Tropin, 2025-04-11] Make it easy to add tags/metainfo
;; to tests to be able to run/skip them flexibly

;; TODO: [Andrew Tropin, 2025-04-22] Add enable-re-run-failed-tests-on-eval,
;; which will re-run last failed tests on each eval
