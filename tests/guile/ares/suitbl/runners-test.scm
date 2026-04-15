;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl runners-test)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl runners)
  #:use-module (ares suitbl exceptions)
  #:use-module ((ares suitbl state) #:prefix state:)
  #:use-module ((ares suitbl running) #:prefix running:)
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
          (is (error "assertions-handling-tests/assert exception")))
        (state:get-run-summary
         (tr `((type . runner/get-state))))))

    (is (= 1 (assoc-ref run-summary 'errors)))
    (is (= 0 (assoc-ref run-summary 'failures)))
    (is (= 1 (assoc-ref run-summary 'assertions)))
    (is (= 1 (assoc-ref run-summary 'tests)))))

(define-suite assertion-run-history-tests
  (test "run history stores assertion runs in source order"
    (define tr (silent-runner))
    (define run-history
      (with-test-runner tr
        (test "history test"
          (is #t)
          (is #f)
          (is (error "assertion-run-history-tests/history test")))
        (state:get-run-history
         (tr `((type . runner/get-state))))))
    (define test-run (car run-history))
    (define assertion-runs
      (assoc-ref test-run 'test-run/assertion-runs))
    (define first-run (car assertion-runs))
    (define second-run (cadr assertion-runs))
    (define third-run (caddr assertion-runs))
    (define first-run-result
      (assoc-ref first-run 'assertion-run/result))
    (define second-run-result
      (assoc-ref second-run 'assertion-run/result))
    (define third-run-result
      (assoc-ref third-run 'assertion-run/result))

    (is (= 3 (length assertion-runs)))
    (is (equal?
         '(#t #f (error "assertion-run-history-tests/history test"))
         (map (lambda (assertion-run)
                (assoc-ref
                 (assoc-ref assertion-run 'assertion)
                 'assert/body))
              assertion-runs)))

    (is (eq? 'pass (assoc-ref first-run 'assertion-run/outcome)))
    (is (running:returned? first-run-result))
    (is (eq? #t (running:returned-value first-run-result)))

    (is (eq? 'fail (assoc-ref second-run 'assertion-run/outcome)))
    (is (running:returned? second-run-result))
    (is (eq? #f (running:returned-value second-run-result)))

    (is (eq? 'error (assoc-ref third-run 'assertion-run/outcome)))
    (is (running:raised? third-run-result))
    (is (equal?
         "assertion-run-history-tests/history test"
         (exception-message
          (running:raised-exception third-run-result))))))

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
    (define tr (silent-runner))
    (with-test-runner tr
      (test "outer test"
        (test "inner test"
          (is #t))))
    (define run-history
      (state:get-run-history (tr `((type . runner/get-state)))))
    (define outer-test-run (car run-history))
    (define test-run-result
      (assoc-ref outer-test-run 'test-run/result))
    (is (running:raised? test-run-result))
    (let ((exception (running:raised-exception test-run-result)))
      (is (suitbl-wrong-position-exception? exception))
      (is (eq? 'test (suitbl-wrong-position-exception-form exception)))
      (is (eq? 'test-body (suitbl-wrong-position-exception-position exception)))
      (is (equal? "Test Macros can't be nested"
                  (exception-message exception)))))

  (test "suite raises suitbl wrong-position exception inside test body"
    (define tr (silent-runner))
    (with-test-runner tr
      (test "outer test"
        (suite "inner suite"
          (is #t))))
    (define run-history
      (state:get-run-history (tr `((type . runner/get-state)))))
    (define outer-test-run (car run-history))
    (define test-run-result
      (assoc-ref outer-test-run 'test-run/result))
    (is (running:raised? test-run-result))
    (let ((exception (running:raised-exception test-run-result)))
      (is (suitbl-wrong-position-exception? exception))
      (is (eq? 'suite (suitbl-wrong-position-exception-form exception)))
      (is (eq? 'test-body (suitbl-wrong-position-exception-position exception)))
      (is (equal? "Test Suite can't be nested into Test Macro"
                  (exception-message exception))))))

(define-suite re-raise-tests
  (test "test body exception is replayed when re-raise is enabled"
    (define counter 0)
    (define tr
      (make-suitbl-test-runner
       #:config `((test-reporter . ,reporter:silent)
                  (re-raise? . #t))))
    (define exception
      (capture-exception
       (lambda ()
         (with-test-runner tr
           (test "replay-check"
             (set! counter (+ counter 1))
             (error "re-raise-tests/test-body replay-check"))))))
    (is exception)
    (is (equal? "re-raise-tests/test-body replay-check"
                (exception-message exception)))
    (is (= 2 counter)))

  (test "is inside test is replayed after test body when re-raise is enabled"
    (define test-body-counter 0)
    (define is-body-counter 0)
    (define after-is-counter 0)
    (define tr
      (make-suitbl-test-runner
       #:config `((test-reporter . ,reporter:silent)
                  (re-raise? . #t))))
    (define exception
      (capture-exception
       (lambda ()
         (with-test-runner tr
           (test "is replay-check"
             (set! test-body-counter (+ test-body-counter 1))
             (is (begin
                   (set! is-body-counter (+ is-body-counter 1))
                   (error "re-raise-tests/is-inside-test")))
             (set! after-is-counter (+ after-is-counter 1)))))))
    (is exception)
    (is (equal? "re-raise-tests/is-inside-test"
                (exception-message exception)))
    (is (= 1 test-body-counter))
    (is (= 2 is-body-counter))
    (is (= 1 after-is-counter)))

  (test "lonely is re-raises exception when re-raise is enabled"
    (define tr
      (make-suitbl-test-runner
       #:config `((test-reporter . ,reporter:silent)
                  (re-raise? . #t))))
    (define exception
      (capture-exception
       (lambda ()
         (with-test-runner tr
           (is (error "re-raise-tests/lonely-is"))))))
    (is exception)
    (is (equal? "re-raise-tests/lonely-is"
                (exception-message exception)))))

