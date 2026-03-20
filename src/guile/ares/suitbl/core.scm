;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl core)
  #:use-module (ares suitbl runners)
  #:use-module (ares suitbl definitions)
  #:export (with-test-runner)
  #:re-export (test-runner*

               is
               test test?
               test-thunk
               suite suite?
               suite-thunk suite-thunk?

               define-suite))

;; Forces to load runners module, so test-runner* is initialized
(define %runners-module-force-load make-suitbl-test-runner)

(define-syntax with-test-runner
  (lambda (stx)
    (syntax-case stx ()
      ((_ test-runner body body* ...)
       #'(parameterize ((test-runner* test-runner))
           body body* ...)))))
