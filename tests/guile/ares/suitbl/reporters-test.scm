;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl reporters-test)
  #:use-module (ares suitbl core)
  #:use-module ((ares suitbl runners)
                #:select (make-suitbl-test-runner))
  #:use-module ((ares suitbl reporters) #:prefix reporter:)
  #:use-module ((ares suitbl state) #:prefix state:))

(define (make-test-node description)
  `((test . ((test/description . ,description)))))

(define (make-suite-node description children)
  `((suite . ((suite/description . ,description)))
    (suite-node/children . ,children)))

(define (make-module-suite-node description children)
  `((suite . ((suite/description . ,description)
              (suite/metadata . ((module-suite? . #t)))))
    (suite-node/children . ,children)))

(define-suite loaded-summary-reporter-tests
  (test "returns #f for unrelated message types"
    (is (not (reporter:loaded-summary
              `((type . reporter/test-start)
                (description . "some test"))))))

  (test "formats the summary output correctly"
    (define port (open-output-string))
    (reporter:loaded-summary
     `((type . reporter/suite-tree-loaded)
       (reporter/port . ,port)
       (suite-node
        . ,(make-module-suite-node "root"
             (list
              (make-test-node "test-a")
              (make-suite-node "inner"
                (list (make-test-node "test-b")
                      (make-test-node "test-c")))
              (make-suite-node "empty" '()))))))
    (is (equal?
         "Loaded 3 tests and 3 suites (1 module, 1 empty).\n"
         (get-output-string port)))))

(define-suite junit-reporter-tests
  (test "returns #f for unrelated message types"
    (is (not (reporter:junit
              `((type . reporter/test-start)
                (description . "some test")))))
    (is (not (reporter:junit
              `((type . reporter/assertion-pass)
                (assert/body . (= 1 1)))))))

  (test "emits JUnit XML on run-end"
    (define port (open-output-string))
    (define test-runner
      (make-suitbl-test-runner
       #:config `((test-reporter . ,reporter:silent))))
    (parameterize ((test-runner* test-runner))
      (suite "sample"
        (test "passing test"
          (is #t)))
      (test-runner `((type . runner/run-tests))))
    (define runner-state
      (test-runner `((type . runner/get-state))))
    (reporter:junit
     `((type . reporter/run-end)
       (reporter/port . ,port)
       (suitbl/state . ,runner-state)))
    (define xml-output (get-output-string port))
    (is (string-contains xml-output "<testsuites"))
    (is (string-contains xml-output "<testsuite"))
    (is (string-contains xml-output "<testcase"))
    (is (string-contains xml-output "passing test")))

  (test "reports failures in JUnit XML"
    (define port (open-output-string))
    (define test-runner
      (make-suitbl-test-runner
       #:config `((test-reporter . ,reporter:silent))))
    (parameterize ((test-runner* test-runner))
      (suite "fail-suite"
        (test "failing test"
          (is #f)))
      (test-runner `((type . runner/run-tests))))
    (define runner-state
      (test-runner `((type . runner/get-state))))
    (reporter:junit
     `((type . reporter/run-end)
       (reporter/port . ,port)
       (suitbl/state . ,runner-state)))
    (define xml-output (get-output-string port))
    (is (string-contains xml-output "failures=\"1\""))
    (is (string-contains xml-output "<failure")))

  (test "reports errors in JUnit XML"
    (define port (open-output-string))
    (define test-runner
      (make-suitbl-test-runner
       #:config `((test-reporter . ,reporter:silent))))
    (parameterize ((test-runner* test-runner))
      (suite "error-suite"
        (test "erroring test"
          (is (throw 'boom))))
      (test-runner `((type . runner/run-tests))))
    (define runner-state
      (test-runner `((type . runner/get-state))))
    (reporter:junit
     `((type . reporter/run-end)
       (reporter/port . ,port)
       (suitbl/state . ,runner-state)))
    (define xml-output (get-output-string port))
    (is (string-contains xml-output "errors=\"1\""))
    (is (string-contains xml-output "<error"))))
