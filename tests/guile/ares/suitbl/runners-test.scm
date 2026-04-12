;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl runners-test)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl runners)
  #:use-module (ares suitbl exceptions)
  #:use-module ((ares suitbl state) #:prefix state:)
  #:use-module ((ares suitbl reporters) #:prefix reporter:)
  #:use-module ((ice-9 exceptions) #:select (exception-message
                                             with-exception-handler)))

(define (silent-runner)
  (make-suitbl-test-runner
   #:config `((test-reporter . ,reporter:silent))))

(define (capture-exception thunk)
  (with-exception-handler
   (lambda (ex)
     ex)
   (lambda ()
     (thunk)
     #f)
   #:unwind? #t))

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

(define-suite wrong-position-tests
  (test "is raises suitbl wrong-position exception inside suite body"
    (define exception
      (capture-exception
       (lambda ()
         (with-test-runner (silent-runner)
           (suite "outer suite"
             (is #t))))))
    (is (suitbl-wrong-position-exception? exception))
    (is (eq? 'is (suitbl-wrong-position-exception-form exception)))
    (is (eq? 'suite-body (suitbl-wrong-position-exception-position exception)))
    (is (equal? "Assert encountered inside suite, but outside of test"
                (exception-message exception))))

  (test "test raises suitbl wrong-position exception inside test body"
    (define exception
      (capture-exception
       (lambda ()
         (with-test-runner (silent-runner)
           (test "outer test"
             (test "inner test"
               (is #t)))))))
    (is (suitbl-wrong-position-exception? exception))
    (is (eq? 'test (suitbl-wrong-position-exception-form exception)))
    (is (eq? 'test-body (suitbl-wrong-position-exception-position exception)))
    (is (equal? "Test Macros can't be nested" (exception-message exception))))

  (test "suite raises suitbl wrong-position exception inside test body"
    (define exception
      (capture-exception
       (lambda ()
         (with-test-runner (silent-runner)
           (test "outer test"
             (suite "inner suite"
               (is #t)))))))
    (is (suitbl-wrong-position-exception? exception))
    (is (eq? 'suite (suitbl-wrong-position-exception-form exception)))
    (is (eq? 'test-body (suitbl-wrong-position-exception-position exception)))
    (is (equal? "Test Suite can't be nested into Test Macro"
                (exception-message exception)))))

