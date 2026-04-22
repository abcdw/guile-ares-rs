;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl specimens-test)
  #:use-module ((ares guile prelude) #:select (comment))
  #:use-module (ares suitbl definitions)
  #:export (all-tests))

;; TODO: [Andrew Tropin, 2026-04-21] Add tests with stdout/stderr
;; TODO: [Andrew Tropin, 2026-04-21] Add inifinite tests


;;;
;;; Specimens
;;;

(define passing-tests
  (suite-thunk "passing tests"
    (test "contains several passing assertions"
      (is (= 4 (+ 2 2)))
      (is (equal? '(a b c d e f g h j k l m n o p q r)
                  (list 'a 'b 'c 'd 'e 'f 'g 'h 'j 'k 'l 'm 'n 'o 'p 'q 'r)))
      (is (string=? "suitbl"
                    (string-append "suit" "bl"))))))

(define failing-tests
  (suite-thunk "failing tests"
    (test "contains a failing assertion among passing ones"
      (is (= 4 (+ 2 2)))
      (is (equal? '(a b c d e f g h j k l m n o p q r)
                  (list 'a 'b 'c 'd 'e 'f 'g 'h 'j 'k 'l 'm 'n 'o 'p 'q 'r)))
      (is (equal? '(a b c d e f g h j k l m n o p q r)
                  (list 'a 'c 'b 'd 'e 'f 'g 'h 'j 'k 'l 'm 'n 'o 'p 'q 'r)))
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
           "failing-tests/assertion and\ntest body exception"))
      (error
       "failing-tests/test body exception\n after assertion error")
      (is (string-prefix? "suit"
                          "suitbl")))))

(define all-tests
  (suite-thunk "all tests"
    'metadata
    '((dev? . #t))
    (passing-tests)
    (failing-tests)))

(define (run-all-tests-with-reporter reporter)
  (define make-suitbl
    (module-ref (resolve-interface '(ares suitbl runner))
                'make-suitbl))
  (define test-runner
    (make-suitbl
     #:config `((auto-run? . #f)
                (test-reporter . ,reporter))))

  (parameterize ((test-runner* test-runner))
    (all-tests)
    (test-runner `((type . runner/run-tests)))))


;;;
;;; Examples
;;;

(comment
 (run-all-tests-with-reporter (@ (ares suitbl reporters) compact))
 (run-all-tests-with-reporter (@ (ares suitbl reporters) minimal))
 (run-all-tests-with-reporter (@ (ares suitbl reporters) base)))
