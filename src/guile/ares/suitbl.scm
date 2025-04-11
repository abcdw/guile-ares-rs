;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl)
  #:export (define-test is))

;; TODO: [Andrew Tropin, 2025-02-19] Look at

;; https://gerbil.scheme.org/reference/dev/test.html
;; https://srfi.schemers.org/srfi-78/srfi-78.html


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


Test runners:
module-patterns to filter modules containing tests.

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


|#


(define test-path* (make-parameter '()))
(define test-case* (make-parameter "unnamed"))

(define-syntax test-case
  (lambda (x)
    "Test case represent a logical unit of testing, can include zero or
more asserts."
    (syntax-case x ()
      ((test-case description expression ...)
       #'(parameterize ((test-case* description))
           ;; TODO: [Andrew Tropin, 2025-04-11] Notify test case
           ;; started (for cases with zero asserts)
           expression ...)))))


(define-syntax test-suite
  (lambda (x)
    "Test suite is simple unit of testing, it can be executed in parallel,
allows to group test cases, can include other test suits."
    (syntax-case x ()
      ((_ description expression ...)
       #'(let ((test-suite-lambda
                (lambda ()
                  (parameterize ((test-path* (cons description (test-path*))))
                    expression ...))))
           (set-procedure-properties!
            test-suite-lambda
            `((name . test-suite)
              (documentation . ,description)
              (srfi-264-test-suite? . #t)))
           test-suite-lambda)))))

(define-syntax define-test-suite
  (syntax-rules ()
    ((_ test-suite-name expression ...)
     (define test-suite-name
       (test-suite (symbol->string 'test-suite-name) expression ...)))))

;; https://cljdoc.org/d/lambdaisland/kaocha/1.91.1392/doc/5-running-kaocha-from-the-repl

(define test-output-port* (make-parameter (current-output-port)))
(define test-do-report* (make-parameter default-report))
(define test-run-assert* (make-parameter default-run-assert))

(define (default-report type params)
  "Default report implementation"
  (case type
    ((pass) (format (test-output-port*) "✓ ~a\n"
                    (assoc-ref params 'expected)))
    ((fail) (format (test-output-port*) "✗~%  Expected: ~a~%  ~a: ~a\n"
                    (assoc-ref params 'expected)
                    (if (assoc-ref params 'error) "Error" "Actual")
                    (or (assoc-ref params 'error)
                        (assoc-ref params 'actual))))
    (else (throw 'no-such-handler))))

(define (default-run-assert form-thunk args-thunk quoted-form)
  (with-exception-handler
   (lambda (ex)
     ((test-do-report*) 'fail
      `((expected . ,'form)
        (error . ,ex))))
   (lambda ()
     ;; TODO: [Andrew Tropin, 2024-12-23] Write down evaluation time
     ;; TODO: [Andrew Tropin, 2024-12-23] Report start before evaling the form
     (let* ((result (form-thunk)))
       ((test-do-report*) (if result 'pass 'fail)
        `((expected . ,quoted-form)
          (actual . (not ,quoted-form))))
       result))
   #:unwind? #t))

;; (lset-difference = '(1 2) '(2 3))
;; (report 'pass '((message . hi)))



(define-syntax is
  (lambda (x)
    (syntax-case x ()
      ((_ (pred args ...))
       (with-syntax ((form #'(pred args ...)))
         #'((test-run-assert*)
            (lambda () form)
            (lambda () (list args ...))
            'form)))
      ((_ form)
       #'((test-run-assert*) (lambda () form) #f 'form)))))

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



(define-test-suite addition
  (is (= 4 (+ 2 2)))
  (is (= 7 (+ 3 4))))

(define-test-suite subtraction
  (is (= 2 (- 4 3)))
  (is (= 3 (- 7 4))))

(define-test-suite exception
  (is (= 3 (throw 'hi))))

(define-test-suite all-tests
  (different-is-usages)
  (addition)
  (subtraction)
  (exception))

;; (all-tests)

;; TODO: [Andrew Tropin, 2025-04-11] Specify test timeouts to 10 by
;; default, so the test evaluation never hangs.

;; TODO: [Andrew Tropin, 2025-04-11] Make it easy to add tags/metainfo
;; to tests to be able to run/skip them flexibly
