;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (srfi-269-test-runner)
  #:use-module (test-suite lib)
  #:export (run-tests))



(define failure-results
  '(fail upass error))

(define (run-tests)
  (define counter
    (make-count-reporter))
  (define successful? #t)

  (define (outcome-reporter result _name . _arguments)
    (when (memq result failure-results)
      (set! successful? #f)))

  (let ((reporters (list (car counter)
                         user-reporter
                         outcome-reporter)))
    (for-each register-reporter reporters)
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (resolve-module '(srfi srfi-269-test)))
      (lambda ()
        (for-each unregister-reporter reporters)))
    (print-counts ((cadr counter)))
    (exit (if successful? 0 1))))
