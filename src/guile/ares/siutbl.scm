;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024 Andrew Tropin <andrew@trop.in>

(define-module (ares siutbl)
  ;; Maybe call it define-suite?
  #:export (define-test is))

#|

Funny name for the library:

SIUTBL Scheme Interactive Unit Testing Base Library

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

How to backlink test to function, so you can see all the tests related
to the function?

|#

(define-syntax define-test
  (syntax-rules ()
    ((_ test-name e ...)
     (begin
       (define-public (test-name) e ...)
       (set-procedure-property! test-name 'srfi-64-test? #t)))))

;; https://cljdoc.org/d/lambdaisland/kaocha/1.91.1392/doc/5-running-kaocha-from-the-repl

(define current-test-port (make-parameter (current-output-port)))

(define (default-report type params)
  "Default report implementation"
  (case type
    ((pass) (format (current-test-port) "passed: ~a\n" params))
    ((fail) (format (current-test-port) "failed: ~a\n" params))
    (else (throw 'no-such-handler))))

(define report (make-parameter default-report))
;; (lset-difference = '(1 2) '(2 3))
;; (report 'pass '((message . hi)))

(define-syntax is
  (syntax-rules ()
    ((_ form)
     (is form ""))
    ((_ form msg)
     (let ((result form))
       ((report) (if result 'pass 'fail)
               `((message . ,msg)
                 (expected . ,'form)
                 (actual . ,result)))
       result))))

(is (lset= = '(1 2 2 3) '(2 3 4 5)))

;; (is (format #t "hello\n"))
(define-test addition
  (is (= 4 (+ 2 2)))
  (is (= 7 (+ 3 4))))

(define-test subtraction
  (is (= 2 (- 4 3)))
  (is (= 3 (- 7 4))))

(define-test arithmetic
  (addition)
  (subtraction))

;; (arithmetic)

(define (assert-expression form message)
  'hey)
