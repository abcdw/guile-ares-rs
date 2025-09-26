;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl runner-state-test)
  #:use-module (ares guile prelude)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl runners)
  #:use-module ((ares suitbl runner-state) #:prefix state:))

(define (get-test-runner-with-sample-suite-loaded)
  (define tr (make-silent-test-runner))

  (with-test-runner tr
    (suite "first suite"
      (test "good one"
        (is #t)
        (is 123))
      (suite "nested-suite"
        (test "failing test"
          (is #f)
          (is (throw 'hi))))
      (test "another good one"
        (is #t))))
    tr)

(define-suite sample-test-suite-state-tests
  (test "test runner has a correct suite forest"
    (define tr (get-test-runner-with-sample-suite-loaded))

    (define state
      (tr `((type . runner/get-state))))

    (define suite-forest
      (state:get-suite-forest state))

    (is (equal?
         '(((suite . "first suite")
            (suite-node/children
             ((test . "good one"))
             ((suite . "nested-suite")
              (suite-node/children ((test . "failing test"))))
             ((test . "another good one")))))
         (state:simplify-suite-forest suite-forest))))

  (test "test runnner has a correct run history"
    (define tr (get-test-runner-with-sample-suite-loaded))

    (define state
      (tr `((type . runner/get-state))))

    (define run-history
      (state:get-run-history state))

    (is (equal?
         '(((test . "another good one")
            (test-run/result
              (tests . 1)
              (failures . 0)
              (errors . 0)
              (skipped . 0)
              (assertions . 1)))
           ((test . "failing test")
            (test-run/result
              (tests . 1)
              (failures . 0)
              (errors . 1)
              (skipped . 0)
              (assertions . 2)))
           ((test . "good one")
            (test-run/result
              (tests . 1)
              (failures . 0)
              (errors . 0)
              (skipped . 0)
              (assertions . 2))))
         (state:simplify-run-history run-history)))))
