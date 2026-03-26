;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl reporters-test)
  #:use-module (ares suitbl core)
  #:use-module ((ares suitbl reporters)
                #:select (count-suites-and-tests
                          test-reporter-loaded-summary
                          test-reporter-output-port*)))

(define (make-test-node description)
  `((test . ((test/description . ,description)))))

(define (make-suite-node description children)
  `((suite . ((suite/description . ,description)))
    (suite-node/children . ,children)))

(define (make-module-suite-node description children)
  `((suite . ((suite/description . ,description)
              (suite/metadata . ((module-suite? . #t)))))
    (suite-node/children . ,children)))

(define-suite count-suites-and-tests-tests
  (test "single test node"
    (define counts (count-suites-and-tests (make-test-node "a test")))
    (is (= 0 (assoc-ref counts 'suites)))
    (is (= 1 (assoc-ref counts 'tests)))
    (is (= 0 (assoc-ref counts 'module-suites)))
    (is (= 0 (assoc-ref counts 'empty-suites))))

  (test "single suite with one test"
    (define counts
      (count-suites-and-tests
       (make-suite-node "s" (list (make-test-node "t")))))
    (is (= 1 (assoc-ref counts 'suites)))
    (is (= 1 (assoc-ref counts 'tests)))
    (is (= 0 (assoc-ref counts 'module-suites)))
    (is (= 0 (assoc-ref counts 'empty-suites))))

  (test "suite with multiple tests"
    (define counts
      (count-suites-and-tests
       (make-suite-node "s"
         (list (make-test-node "t1")
               (make-test-node "t2")
               (make-test-node "t3")))))
    (is (= 1 (assoc-ref counts 'suites)))
    (is (= 3 (assoc-ref counts 'tests))))

  (test "nested suites"
    (define counts
      (count-suites-and-tests
       (make-suite-node "outer"
         (list
          (make-suite-node "inner"
            (list (make-test-node "t1")
                  (make-test-node "t2")))))))
    (is (= 2 (assoc-ref counts 'suites)))
    (is (= 2 (assoc-ref counts 'tests))))

  (test "empty suite"
    (define counts
      (count-suites-and-tests
       (make-suite-node "empty" '())))
    (is (= 1 (assoc-ref counts 'suites)))
    (is (= 0 (assoc-ref counts 'tests)))
    (is (= 1 (assoc-ref counts 'empty-suites))))

  (test "module suite"
    (define counts
      (count-suites-and-tests
       (make-module-suite-node "mod"
         (list (make-test-node "t")))))
    (is (= 1 (assoc-ref counts 'suites)))
    (is (= 1 (assoc-ref counts 'tests)))
    (is (= 1 (assoc-ref counts 'module-suites))))

  (test "complex tree with nested, module, and empty suites"
    ;; root-suite (module)
    ;;   ├── test-a
    ;;   ├── inner-suite
    ;;   │   ├── test-b
    ;;   │   └── test-c
    ;;   └── empty-suite
    (define counts
      (count-suites-and-tests
       (make-module-suite-node "root"
         (list
          (make-test-node "test-a")
          (make-suite-node "inner"
            (list (make-test-node "test-b")
                  (make-test-node "test-c")))
          (make-suite-node "empty" '())))))
    (is (= 3 (assoc-ref counts 'suites)))
    (is (= 3 (assoc-ref counts 'tests)))
    (is (= 1 (assoc-ref counts 'module-suites)))
    (is (= 1 (assoc-ref counts 'empty-suites)))))

(define-suite loaded-summary-reporter-tests
  (test "returns #f for unrelated message types"
    (is (not (test-reporter-loaded-summary
              `((type . reporter/test-start)
                (description . "some test"))))))

  (test "formats the summary output correctly"
    (define port (open-output-string))
    (parameterize ((test-reporter-output-port* port))
      (test-reporter-loaded-summary
       `((type . reporter/suite-tree-loaded)
         (suite-node
          . ,(make-module-suite-node "root"
               (list
                (make-test-node "test-a")
                (make-suite-node "inner"
                  (list (make-test-node "test-b")
                        (make-test-node "test-c")))
                (make-suite-node "empty" '())))))))
    (is (equal?
         "Loaded 3 tests and 3 suites (1 module, 1 empty).\n"
         (get-output-string port)))))
