;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl running)
  #:use-module ((srfi srfi-1) #:select (count))
  #:export (assertion-events->summary
            assertion-summary->test-run-status
            assertion-events->test-run-summary))


;;;
;;; Running helpers
;;;

(define (assertion-events->summary events)
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

(define (assertion-events->test-run-summary events)
  (let* ((assertion-summary (assertion-events->summary events))
         (result (assertion-summary->test-run-status assertion-summary)))
    `((tests . 1)
      (failures . ,(if (eq? result 'fail) 1 0))
      (errors . ,(if (eq? result 'error) 1 0))
      (skipped . 0)
      (assertions . ,(assoc-ref assertion-summary 'assertions)))))
