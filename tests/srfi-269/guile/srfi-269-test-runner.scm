;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (srfi-269-test-runner)
  #:use-module ((ares suitbl runner) #:prefix runner:)
  #:use-module ((ares suitbl state) #:prefix state:)
  #:use-module ((srfi srfi-269) #:prefix srfi-269:)
  #:use-module (srfi srfi-269-test)
  #:export (run-tests))



(define (successful-run? summary)
  (and summary
       (zero? (+ (or (assoc-ref summary 'failures) 0)
                 (or (assoc-ref summary 'errors) 0)))))

(define (run-tests)
  (define test-runner
    (runner:make-suitbl))

  (parameterize ((srfi-269:current-test-runner test-runner))
    (srfi-269-tests)
    (test-runner '((type . runner/run-tests))))

  (unless (successful-run?
           (state:get-run-summary (runner:get-state test-runner)))
    (exit 1)))
