;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl runners-test)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl runners)
  #:use-module ((ares suitbl state) #:prefix state:)
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
         is-values)))

  (test "assert exception is reported as error"
    (define tr (silent-runner))
    (define run-summary
      (with-test-runner tr
        (test "assert exception"
          (is (error "boom")))
        (state:get-run-summary
         (tr `((type . runner/get-state))))))

    (is (= 1 (assoc-ref run-summary 'errors)))
    (is (= 0 (assoc-ref run-summary 'failures)))
    (is (= 1 (assoc-ref run-summary 'assertions)))
    (is (= 1 (assoc-ref run-summary 'tests)))))

