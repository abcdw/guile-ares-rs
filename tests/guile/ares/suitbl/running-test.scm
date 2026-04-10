;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl running-test)
  #:use-module (ares suitbl core)
  #:use-module ((ice-9 exceptions) #:select (exception-message
                                              with-exception-handler))
  #:use-module (ice-9 match)
  #:use-module ((ares suitbl running) #:prefix running:))



(define-suite assertion-events->assertion-summary-tests
  (test "summarize assertion events with pass-only data"
    (is (equal?
         '((passes . 2)
           (failures . 0)
           (errors . 0)
           (assertions . 2))
         (running:assertion-events->assertion-summary '(pass pass)))))

  (test "summarize assertion events with mixed data"
    (is (equal?
         '((passes . 1)
           (failures . 1)
           (errors . 1)
           (assertions . 3))
         (running:assertion-events->assertion-summary '(pass fail error)))))

  (test "summarize assertion events with empty data"
    (is (equal?
         '((passes . 0)
           (failures . 0)
           (errors . 0)
           (assertions . 0))
         (running:assertion-events->assertion-summary '())))))

(define-suite assertion-summary->test-run-status-tests
  (test "returns pass for zero-assertion summary"
    (is (eq?
         'pass
         (running:assertion-summary->test-run-status
          '((passes . 0)
            (failures . 0)
            (errors . 0)
            (assertions . 0))))))

  (test "returns pass for pass-only summary"
    (is (eq?
         'pass
         (running:assertion-summary->test-run-status
          '((passes . 2)
            (failures . 0)
            (errors . 0)
            (assertions . 2))))))

  (test "returns fail for summary with failures"
    (is (eq?
         'fail
         (running:assertion-summary->test-run-status
          '((passes . 1)
            (failures . 1)
            (errors . 0)
            (assertions . 2))))))

  (test "returns error for summary with errors"
    (is (eq?
         'error
         (running:assertion-summary->test-run-status
          '((passes . 1)
            (failures . 0)
            (errors . 1)
            (assertions . 2))))))

  (test "returns error when both failures and errors are present"
    (is (eq?
         'error
         (running:assertion-summary->test-run-status
          '((passes . 0)
            (failures . 1)
            (errors . 1)
            (assertions . 2)))))))

(define-suite assertion-events->test-run-summary-tests
  (test "returns pass summary for pass-only events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 0)
           (skipped . 0)
           (assertions . 2))
         (running:assertion-events->test-run-summary '(pass pass)))))

  (test "returns fail summary for fail-only events"
    (is (equal?
         '((tests . 1)
           (failures . 1)
           (errors . 0)
           (skipped . 0)
           (assertions . 1))
         (running:assertion-events->test-run-summary '(fail)))))

  (test "returns error summary for error-only events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 1)
           (skipped . 0)
           (assertions . 1))
         (running:assertion-events->test-run-summary '(error)))))

  (test "returns error summary for mixed fail and error events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 1)
           (skipped . 0)
           (assertions . 3))
         (running:assertion-events->test-run-summary '(pass fail error)))))

  (test "returns pass summary for empty events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 0)
           (skipped . 0)
           (assertions . 0))
         (running:assertion-events->test-run-summary '())))))

(define (raises-exception? thunk)
  (with-exception-handler
   (lambda (_)
     #t)
   (lambda ()
     (thunk)
     #f)
   #:unwind? #t))

(define-suite with-exception-continuation-tests
  (test "returns tagged returned value when no exception is raised"
    (let ((result
           (running:with-exception-continuation
            (lambda () 'ok))))
      (is (running:returned? result))
      (is (equal? 'ok (running:returned-value result)))))

  (test "returns raised with continuation and exception when thunk raises"
    (let ((result
           (running:with-exception-continuation
            (lambda ()
              (error "boom")))))
      (is (running:raised? result))
      (is (procedure? (running:raised-continuation result)))
      (is (string=? "boom"
                    (exception-message (running:raised-exception result))))))

  (test "captured continuation re-raises exception when called"
    (let ((result
           (running:with-exception-continuation
            (lambda ()
              (error "boom")))))
      (is (running:raised? result))
      (is (raises-exception? (running:raised-continuation result))))))
