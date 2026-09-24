;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares reflection modules-test)
  #:use-module (ares reflection modules)
  #:use-module ((srfi srfi-64)
                #:select (test-group test-equal test-eq))
  #:use-module (test-utils))

(define-test test-string->resolved-module
  (test-group "string->resolved-module"
    (test-equal "resolves a Guile module name"
      '(ares reflection modules)
      (module-name
       (string->resolved-module "(ares reflection modules)")))
    (test-equal "resolves an R7RS SRFI library name"
      '(srfi srfi-1)
      (module-name (string->resolved-module "(srfi 1)")))
    (test-eq "rejects numeric components in other module names"
      #f
      (string->resolved-module "(ares 269)"))
    (test-eq "rejects a missing module name"
      #f
      (string->resolved-module #f))))
