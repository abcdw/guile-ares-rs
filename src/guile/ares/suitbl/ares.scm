;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl ares)
  #:use-module ((ares suitbl core) #:select (suite test-runner*))
  #:use-module ((ares suitbl runners)
                #:select (get-stats set-run-config-value!))
  #:use-module ((ares suitbl discovery)
                #:select (get-all-test-modules get-module-public-suites))
  #:use-module ((srfi srfi-1) #:select (append-map))
  #:use-module ((srfi srfi-197) #:select (chain))
  #:export (run-tests
            get-current-test-runner-stats
            load-project-tests))

(define (run-tests)
  ((test-runner*) `((type . run-tests))))

(define (get-current-test-runner-stats)
  (chain ((test-runner*) `((type . get-state)))
    (get-stats _)))

(define (load-project-tests)
  (set-run-config-value! ((test-runner*) `((type . get-state))) 'auto-run? #f)
  (let ((test-modules (get-all-test-modules)))
    (suite "project tests"
      (for-each
       (lambda (ts) (ts))
       (append-map get-module-public-suites test-modules))))
  (set-run-config-value! ((test-runner*) `((type . get-state))) 'auto-run? #t)
  *unspecified*)
