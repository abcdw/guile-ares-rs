;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl running)
  #:use-module ((srfi srfi-1) #:select (count))
  #:export (summarize-assertion-events
            assertion-summary->test-run-status
            summarize-test-run-events))


;;;
;;; Running helpers
;;;

(define (summarize-assertion-events events)
  `((passes . ,(count (lambda (x) (eq? x 'pass)) events))
    (failures . ,(count (lambda (x) (eq? x 'fail)) events))
    (errors . ,(count (lambda (x) (eq? x 'error)) events))
    (assertions . ,(length events))))

(define (assertion-summary->test-run-status assertion-summary)
  (let ((error? (> (assoc-ref assertion-summary 'errors) 0))
        (fail? (> (assoc-ref assertion-summary 'failures) 0)))
    (cond
     (error? 'error)
     (fail? 'fail)
     (else 'pass))))

(define (summarize-test-run-events events)
  (let* ((assertion-summary (summarize-assertion-events events))
         (result (assertion-summary->test-run-status assertion-summary)))
    `((tests . 1)
      (failures . ,(if (eq? result 'fail) 1 0))
      (errors . ,(if (eq? result 'error) 1 0))
      (skipped . 0)
      (assertions . ,(assoc-ref assertion-summary 'assertions)))))
