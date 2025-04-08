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
Test suites: unit, integration, acceptance, backend, frontend

3.
Watch? for changed tests/implementations?

4.
Arguments pre-evaluations, do we really need it?
Maybe it's ok to do post-fail re-evaluation?

|#



(define-syntax define-test
  (syntax-rules ()
    ((_ test-name e ...)
     (begin
       (define-public (test-name) e ...)
       (set-procedure-property! test-name 'srfi-264-test? #t)))))

;; https://cljdoc.org/d/lambdaisland/kaocha/1.91.1392/doc/5-running-kaocha-from-the-repl

(define current-test-port (make-parameter (current-output-port)))

(define (default-report type params)
  "Default report implementation"
  (case type
    ((pass) (format (current-test-port) "✓ ~a\n"
                    (assoc-ref params 'expected)))
    ((fail) (format (current-test-port) "✗~%  Expected: ~a~%  ~a: ~a\n"
                    (assoc-ref params 'expected)
                    (if (assoc-ref params 'error) "Error" "Actual")
                    (or (assoc-ref params 'error)
                        (assoc-ref params 'actual))))
    (else (throw 'no-such-handler))))

(define (default-run-assert thunk form)
  (with-exception-handler
   (lambda (ex)
     ((report) 'fail
      `((expected . ,'form)
        (error . ,ex))))
   (lambda ()
     ;; TODO: [Andrew Tropin, 2024-12-23] Write down evaluation time
     ;; TODO: [Andrew Tropin, 2024-12-23] Report start before evaling the form
     (let* ((result (thunk)))
       ((report) (if result 'pass 'fail)
        `((expected . ,form)
          (actual . (not ,form))))
       result))
   #:unwind? #t))

(define report (make-parameter default-report))
(define run-assert (make-parameter default-run-assert))

;; (lset-difference = '(1 2) '(2 3))
;; (report 'pass '((message . hi)))



#|

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

(define-syntax try-expression
  (lambda (x)
    (syntax-case x ()
      ;; TODO: [Andrew Tropin, 2025-04-08] Doesn't work for macros
      ;; TODO: [Andrew Tropin, 2025-04-08] Argument pre-evaluation can
      ;; throw an error
      ((_ (pred args ...))
       #'(let* ((evaluated-args (list args ...))
                (form `(pred args ...))
                (result-thunk (lambda () (apply pred evaluated-args))))
           ((run-assert) result-thunk form)))
      ((_ form)
       #'(let ((result-thunk (lambda () form)))
           ((run-assert) result-thunk 'form))))))

;; (apply begin (list 'hi 'ho))

(define-syntax is
  (syntax-rules ()
    ((_ form)
     (try-expression form))))

(define-test different-is-usages
  (is #t)
  (define a 123)
  (is a)
  (is #f)
  (is (lset= = '(1 2 2 3) '(2 3 4 5)))
  ;; (is (begin 'value1 'value2))
  ;; (is (= (throw 'hi) 7))

  (is (= 4 7))
  (is (= 4 (+ 2 2))))

;; (different-is-usages)

(define-test checking-custom-try-expression
  (is (= 4 (+ 2 2)))
  (is (= 7 (+ 3 4))))

;; (syntax-parameterize ((try-expression custom-try-expression))
;;                      (checking-custom-try-expression))

(define-test addition
  (is (= 4 (+ 2 2)))
  (is (= 7 (+ 3 4))))

(define-test subtraction
  (is (= 2 (- 4 3)))
  (is (= 3 (- 7 4))))

(define-test exception
  (is (= 3 (throw 'hi))))

(define-test all-tests
  (addition)
  (subtraction)
  (exception))

;; (all-tests)
;; TODO: [Andrew Tropin, 2024-12-23] Throw an exception

(define (assert-expression form message)
  'hey)

;; Idea:
;; Is as a syntax parameter
