;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl runner-state-test)
  #:use-module (ares guile prelude)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl runners)
  #:use-module ((ares suitbl runner-state) #:prefix state:)
  #:use-module ((srfi srfi-1) #:select (filter)))

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

    (define expected-simplified-history
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
          (assertions . 2)))))

    (is (equal?
         expected-simplified-history
         (state:simplify-run-history run-history)))))

(define-suite run-summarization-tests
  (test "run forest has run result summary attached to each node"
    (define tr (get-test-runner-with-sample-suite-loaded))

    (define state
      (tr `((type . runner/get-state))))

    (define run-forest-with-summary
      (state:get-suite-forest-with-summary state))

    (define expected-simplified-forest
      '(((suite . "first suite")
         (suite-node/children
          ((test . "good one")
           (test-run/result
            (tests . 1)
            (failures . 0)
            (errors . 0)
            (skipped . 0)
            (assertions . 2)))
          ((suite . "nested-suite")
           (suite-node/children
            ((test . "failing test")
             (test-run/result
              (tests . 1)
              (failures . 0)
              (errors . 1)
              (skipped . 0)
              (assertions . 2))))
           (suite-run/result
            (tests . 1)
            (failures . 0)
            (errors . 1)
            (skipped . 0)
            (assertions . 2)))
          ((test . "another good one")
           (test-run/result
            (tests . 1)
            (failures . 0)
            (errors . 0)
            (skipped . 0)
            (assertions . 1))))
         (suite-run/result
          (tests . 3)
          (failures . 0)
          (errors . 1)
          (skipped . 0)
          (assertions . 5)))))

    (is (equal?
         expected-simplified-forest
         (state:simplify-suite-forest run-forest-with-summary)))))

(define-suite schedule-tests-tests
  (test "all loaded tests are scheduled when no schedule-tests in config"
    (define tr (get-test-runner-with-sample-suite-loaded))
    (define state (tr `((type . runner/get-state))))
    (define loaded (state:get-loaded-tests state))
    (define scheduled (state:get-scheduled-tests state '()))
    (is (equal? loaded scheduled)))

  (test "custom schedule-tests filters tests"
    (define tr (get-test-runner-with-sample-suite-loaded))
    (define state (tr `((type . runner/get-state))))
    (define config
      `((schedule-tests
         . ,(lambda (tests state)
               (filter (lambda (t)
                         (equal? "good one"
                                 (assoc-ref t 'test/description)))
                       tests)))))
    (define scheduled (state:get-scheduled-tests state config))
    (is (= 1 (length scheduled)))
    (is (equal? "good one"
                (assoc-ref (car scheduled) 'test/description))))

  (test "schedule-tests can return empty list"
    (define tr (get-test-runner-with-sample-suite-loaded))
    (define state (tr `((type . runner/get-state))))
    (define config
      `((schedule-tests . ,(lambda (tests state) '()))))
    (define scheduled (state:get-scheduled-tests state config))
    (is (null? scheduled)))

  (test "get-stats reflects scheduling"
    (define tr (get-test-runner-with-sample-suite-loaded))
    (define state (tr `((type . runner/get-state))))
    (define config
      `((schedule-tests
         . ,(lambda (tests state)
               (filter (lambda (t)
                         (equal? "good one"
                                 (assoc-ref t 'test/description)))
                       tests)))))
    (define stats (state:get-stats state config))
    (is (= 3 (assoc-ref stats 'loaded-tests-count)))
    (is (= 1 (assoc-ref stats 'selected-tests-count))))

  (test "get-stats with default scheduling shows all tests selected"
    (define tr (get-test-runner-with-sample-suite-loaded))
    (define state (tr `((type . runner/get-state))))
    (define stats (state:get-stats state '()))
    (is (= 3 (assoc-ref stats 'loaded-tests-count)))
    (is (= 3 (assoc-ref stats 'selected-tests-count)))))
