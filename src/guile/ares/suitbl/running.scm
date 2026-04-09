;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl running)
  #:use-module ((srfi srfi-1) #:select (count))
  #:export (summarize-assertion-events
            summarize-test-run-events))


;;;
;;; Running helpers
;;;

(define (summarize-assertion-events events)
  `((passes . ,(count (lambda (x) (eq? x 'pass)) events))
    (failures . ,(count (lambda (x) (eq? x 'fail)) events))
    (errors . ,(count (lambda (x) (eq? x 'error)) events))
    (assertions . ,(length events))))

(define (summarize-test-run-events events)
  (let* ((assertion-summary (summarize-assertion-events events))
         (error? (> (assoc-ref assertion-summary 'errors) 0))
         (fail? (> (assoc-ref assertion-summary 'failures) 0)))
    `((tests . 1)
      (failures . ,(if (and fail? (not error?)) 1 0))
      (errors . ,(if error? 1 0))
      (skipped . 0)
      (assertions . ,(assoc-ref assertion-summary 'assertions)))))
