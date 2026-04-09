;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl runners-test)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl runners)
  #:use-module ((ares suitbl reporters) #:prefix reporter:))

(define (silent-runner)
  (make-suitbl-test-runner
   #:config `((test-reporter . ,reporter:silent))))

(define-suite assertions-handling-tests
  (test "is assert returns the value of its body"
    (define tr (silent-runner))
    (define is-values
      (with-test-runner tr
        (define b 'heyhey)
        (list
         (is #t)
         (is 123)
         (is 'some-symbol)

         (let ((a 123))
           (is a))
         (is b)

         (is (+ 2 3))
         (is (= 2 3)))))

    (is (equal?
         '(#t 123 some-symbol 123 heyhey 5 #f)
         is-values))))



(define-suite summarize-test-run-events-tests
  (test "summarize pass-only events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 0)
           (skipped . 0)
           (assertions . 2))
         (summarize-test-run-events '(pass pass)))))

  (test "summarize fail-only events"
    (is (equal?
         '((tests . 1)
           (failures . 1)
           (errors . 0)
           (skipped . 0)
           (assertions . 1))
         (summarize-test-run-events '(fail)))))

  (test "summarize error-only events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 1)
           (skipped . 0)
           (assertions . 1))
         (summarize-test-run-events '(error)))))

  (test "summarize mixed fail and error events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 1)
           (skipped . 0)
           (assertions . 3))
         (summarize-test-run-events '(pass fail error)))))

  (test "summarize empty events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 0)
           (skipped . 0)
           (assertions . 0))
         (summarize-test-run-events '())))))
