;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl running)
  #:use-module ((srfi srfi-1) #:select (any))
  #:export (summarize-test-run-events))


;;;
;;; Running helpers
;;;

(define (summarize-test-run-events events)
  (let* ((error? (any (lambda (x) (eq? x 'error)) events))
         (fail? (any (lambda (x) (eq? x 'fail)) events)))
    `((tests . 1)
      (failures . ,(if (and fail? (not error?)) 1 0))
      (errors . ,(if error? 1 0))
      (skipped . 0)
      (assertions . ,(length events)))))
