;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (srfi srfi-269)
  #:export (current-test-runner
            set-default-test-runner!

            is
            test test?
            test-loader
            suite suite?
            suite-loader suite-loader?

            define-suite))



;;;
;;; Test Definitions API
;;;

;; TODO: Implement the Guile-specific SRFI-269 test definition API.
