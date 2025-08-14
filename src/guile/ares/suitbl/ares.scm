;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl ares)
  #:use-module (ares suitbl)
  #:use-module (srfi srfi-197)
  #:export (run-tests
            get-current-test-runner-stats))

(define (run-tests)
  ((test-runner*) `((type . run-tests))))

(define (get-current-test-runner-stats)
  (chain ((test-runner*) `((type . get-state)))
    (get-stats _)))
