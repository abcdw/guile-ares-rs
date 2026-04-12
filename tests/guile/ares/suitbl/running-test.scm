;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl running-test)
  #:use-module (ares suitbl core)
  #:use-module (ice-9 control)
  #:use-module ((ice-9 exceptions) #:select (exception-message
                                              with-exception-handler))
  #:use-module (ice-9 match)
  #:use-module ((ares suitbl running) #:prefix running:))



(define-suite assertion-outcomes->assertion-summary-tests
  (test "summarize assertion outcomes with pass-only data"
    (is (equal?
         '((passes . 2)
           (failures . 0)
           (errors . 0)
           (assertions . 2))
         (running:assertion-outcomes->assertion-summary '(pass pass)))))

  (test "summarize assertion outcomes with mixed data"
    (is (equal?
         '((passes . 1)
           (failures . 1)
           (errors . 1)
           (assertions . 3))
         (running:assertion-outcomes->assertion-summary '(pass fail error)))))

  (test "summarize assertion outcomes with empty data"
    (is (equal?
         '((passes . 0)
           (failures . 0)
           (errors . 0)
           (assertions . 0))
         (running:assertion-outcomes->assertion-summary '())))))

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

(define-suite assertion-outcomes->test-run-summary-tests
  (test "returns pass summary for pass-only events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 0)
           (skipped . 0)
           (assertions . 2))
         (running:assertion-outcomes->test-run-summary '(pass pass)))))

  (test "returns fail summary for fail-only events"
    (is (equal?
         '((tests . 1)
           (failures . 1)
           (errors . 0)
           (skipped . 0)
           (assertions . 1))
         (running:assertion-outcomes->test-run-summary '(fail)))))

  (test "returns error summary for error-only events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 1)
           (skipped . 0)
           (assertions . 1))
         (running:assertion-outcomes->test-run-summary '(error)))))

  (test "returns error summary for mixed fail and error events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 1)
           (skipped . 0)
           (assertions . 3))
         (running:assertion-outcomes->test-run-summary '(pass fail error)))))

  (test "returns pass summary for empty events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 0)
           (skipped . 0)
           (assertions . 0))
         (running:assertion-outcomes->test-run-summary '())))))

(define (raises-exception? thunk)
  (with-exception-handler
   (lambda (_)
     #t)
   (lambda ()
     (thunk)
     #f)
   #:unwind? #t))


;;;
;;; Stack capture helpers
;;;

(define (exception-stack-frame-procedure-names thunk)
  (let/ec return
    (with-exception-handler
     (lambda (_)
       (return
        (let ((stack (make-stack #t)))
          (let loop ((index 0)
                     (result '()))
            (if (= index (stack-length stack))
                (reverse result)
                (loop (1+ index)
                      (cons (frame-procedure-name (stack-ref stack index))
                            result)))))))
     thunk
     #:unwind? #f)))

(define (list-prefix? prefix lst)
  (cond
   ((null? prefix) #t)
   ((null? lst) #f)
   ((equal? (car prefix) (car lst))
    (list-prefix? (cdr prefix) (cdr lst)))
   (else
    #f)))

(define (contains-contiguous-sublist? lst sublist)
  (cond
   ((null? sublist) #t)
   ((null? lst) #f)
   ((list-prefix? sublist lst) #t)
   (else
    (contains-contiguous-sublist? (cdr lst) sublist))))

(define-suite assertion-run-result->assertion-outcome-tests
  (test "maps truthy returned result to pass outcome"
    (let* ((run-result
            (running:with-exception-continuation
             (lambda () #t)))
           (outcome
            (running:assertion-run-result->assertion-outcome run-result)))
      (is (equal? 'pass outcome))))

  (test "maps falsey returned result to fail outcome"
    (let* ((run-result
            (running:with-exception-continuation
             (lambda () #f)))
           (outcome
            (running:assertion-run-result->assertion-outcome run-result)))
      (is (equal? 'fail outcome))))

  (test "maps raised result to error outcome"
    (let* ((run-result
            (running:with-exception-continuation
             (lambda ()
               (error "boom"))))
           (outcome
            (running:assertion-run-result->assertion-outcome run-result)))
      (is (equal? 'error outcome)))))

(define-suite assertion-run-result->reporter-message-tests
  (test "maps truthy returned result to assertion-pass reporter message"
    (let* ((run-result
            (running:with-exception-continuation
             (lambda () #t)))
           (message
            (running:assertion-run-result->reporter-message run-result)))
      (is (equal? 'run/assertion-pass (assoc-ref message 'type)))
      (is (equal? #t (assoc-ref message 'assertion/result)))))

  (test "maps falsey returned result to assertion-fail reporter message"
    (let* ((run-result
            (running:with-exception-continuation
             (lambda () #f)))
           (message
            (running:assertion-run-result->reporter-message run-result)))
      (is (equal? 'run/assertion-fail (assoc-ref message 'type)))
      (is (equal? #f (assoc-ref message 'assertion/result)))))

  (test "maps raised result to assertion-error reporter message"
    (let* ((run-result
            (running:with-exception-continuation
             (lambda ()
               (error "boom"))))
           (message
            (running:assertion-run-result->reporter-message run-result)))
      (is (equal? 'run/assertion-error (assoc-ref message 'type)))
      (is (equal? (running:raised-exception run-result)
                  (assoc-ref message 'assertion/error))))))

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
      (is (raises-exception? (running:raised-continuation result)))))

  (test "captured continuation re-raises exception with original stack"
    (define top #f)
    (define mid #f)
    (define bot #f)
    (define expected-stack-procedure-names
      '(bot mid top))

    ;; Two tricks to avoid inlining optimization of the function
    ;; described in docs/guile/function-inlining.md
    (set! bot (lambda () (error "boom")))
    (set! mid (lambda () (list (bot))))
    (set! top (lambda () (list (mid))))

    (let* ((direct-stack
            (exception-stack-frame-procedure-names top))
           (result
            (running:with-exception-continuation top))
           (reraised-stack
            (exception-stack-frame-procedure-names
             (running:raised-continuation result))))
      (is (running:raised? result))
      (is (contains-contiguous-sublist?
           direct-stack
           expected-stack-procedure-names))
      (is (contains-contiguous-sublist?
           reraised-stack
           expected-stack-procedure-names)))))
