;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl running)
  #:use-module ((srfi srfi-1) #:select (count))
  #:use-module ((ice-9 exceptions) #:select (with-exception-handler))
  #:export (with-exception-continuation
            assertion-events->assertion-summary
            assertion-summary->test-run-status
            assertion-events->test-run-summary))


;;;
;;; Running helpers
;;;

(define (with-exception-continuation thunk)
  "Run THUNK and return a tagged result.

Returns:
- (value . RESULT), when THUNK succeeds.
- (exception-continuation . K), when THUNK raises an exception,
  where K is a continuation captured at the exception point."
  (call/cc
   (lambda (return)
     (with-exception-handler
      (lambda (_)
        (call/cc
         (lambda (continuation)
           (return (cons 'exception-continuation continuation)))))
      (lambda ()
        (cons 'value (thunk)))
      #:unwind? #f))))

(define (assertion-events->assertion-summary events)
  `((passes . ,(count (lambda (x) (eq? x 'pass)) events))
    (failures . ,(count (lambda (x) (eq? x 'fail)) events))
    (errors . ,(count (lambda (x) (eq? x 'error)) events))
    (assertions . ,(length events))))

(define (assertion-summary->test-run-status assertion-summary)
  "Convert ASSERTION-SUMMARY alist into a test run status symbol.

Returns one of: 'pass, 'fail, or 'error.  If both failures and errors
are present, test run status is considered 'error.

Zero assertion means pass."
  (let ((error? (> (assoc-ref assertion-summary 'errors) 0))
        (fail? (> (assoc-ref assertion-summary 'failures) 0)))
    (cond
     (error? 'error)
     (fail? 'fail)
     (else 'pass))))

(define (assertion-events->test-run-summary events)
  (let* ((assertion-summary (assertion-events->assertion-summary events))
         (result (assertion-summary->test-run-status assertion-summary)))
    `((tests . 1)
      (failures . ,(if (eq? result 'fail) 1 0))
      (errors . ,(if (eq? result 'error) 1 0))
      (skipped . 0)
      (assertions . ,(assoc-ref assertion-summary 'assertions)))))
