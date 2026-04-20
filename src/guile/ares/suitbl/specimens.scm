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

(define failing-tests
  (suite-thunk "failing tests"
    (test "contains a failing assertion among passing ones"
      (is (= 4 (+ 2 2)))
      (is (equal? '(a b c)
                  (list 'a 'c 'b)))
      (is (string=? "suitbl"
                    (string-append "suit" "bl"))))

    (test "contains an erroring assertion among passing ones"
      (is (pair? '(a b c)))
      (is (error "failing-tests/erroring assertion"))
      (is (string? "still reached after error")))

    (test "contains an exception in the middle outside assertions"
      (is (= 4 (+ 2 2)))
      (error "failing-tests/test body exception")
      (is (string-prefix? "suit"
                          "suitbl")))

    (test "contains a failing assertion and then an exception outside assertions"
      (is (equal? '(a b c)
                  (list 'a 'b 'c)))
      (is (= 5 (+ 2 2)))
      (error "failing-tests/failure and test body exception")
      (is (string-prefix? "suit"
                          "suitbl")))

    (test "contains an erroring assertion and then an exception outside assertions"
      (is (= 4 (+ 2 2)))
      (is (error
           "failing-tests/assertion and test body exception"))
      (error
       "failing-tests/test body exception after assertion error")
      (is (string-prefix? "suit"
                          "suitbl")))))

(define all-tests
  (suite-thunk "all tests"
    (passing-tests)
    (failing-tests)))
