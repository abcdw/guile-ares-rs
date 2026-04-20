;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl specimens)
  #:use-module (ares suitbl definitions)
  #:export ())


;;;
;;; Specimens
;;;

(define passing-tests
  (suite-thunk "passing tests"
    (test "contains several passing assertions"
      (is (= 4 (+ 2 2)))
      (is (equal? '(a b c)
                  (list 'a 'b 'c)))
      (is (string=? "suitbl"
                    (string-append "suit" "bl"))))))
