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

(define-suite reporter-every-tests
  (test "returns #t when at least one reporter succeeds"
    (define combined
      (reporter:reporter-every
       (list (lambda (msg) #f)
             (lambda (msg) #t)
             (lambda (msg) #f))))
    (is (eq? #t (combined '((type . test))))))

  (test "returns #f when all reporters return #f"
    (define combined
      (reporter:reporter-every
       (list (lambda (msg) #f)
             (lambda (msg) #f))))
    (is (not (combined '((type . test))))))

  (test "calls every reporter even if one succeeds early"
    (define call-log '())
    (define combined
      (reporter:reporter-every
       (list (lambda (msg) (set! call-log (cons 'a call-log)) #t)
             (lambda (msg) (set! call-log (cons 'b call-log)) #f)
             (lambda (msg) (set! call-log (cons 'c call-log)) #t))))
    (combined '((type . test)))
    (is (equal? '(c b a) call-log))))

(define-suite load-ignore-messages-tests
  (test "returns #t for load/test"
    (is (eq? #t
             (reporter:load-ignore-messages
              `((type . load/test)
                (description . "some test"))))))

  (test "returns #t for load/suite-enter"
    (is (eq? #t
             (reporter:load-ignore-messages
              `((type . load/suite-enter)
                (description . "suite"))))))

  (test "returns #t for load/suite-leave"
    (is (eq? #t
             (reporter:load-ignore-messages
              `((type . load/suite-leave)
                (description . "suite"))))))

  (test "returns #f for unrelated message types"
    (is (not (reporter:load-ignore-messages
              `((type . run/start)))))))

(define-suite load-summary-reporter-tests
  (test "returns #f for unrelated message types"
    (is (not (reporter:load-summary
              `((type . run/test-start)
                (description . "some test"))))))

  (test "formats the summary output correctly"
    (define port (open-output-string))
    (reporter:load-summary
     `((type . load/end)
       (reporting/port . ,port)
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
              `((type . run/test-start)
                (description . "some test")))))
    (is (not (reporter:junit
              `((type . run/assertion-pass)
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
     `((type . run/end)
       (reporting/port . ,port)
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
     `((type . run/end)
       (reporting/port . ,port)
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
     `((type . run/end)
       (reporting/port . ,port)
       (suitbl/state . ,runner-state)))
    (define xml-output (get-output-string port))
    (is (string-contains xml-output "errors=\"1\""))
    (is (string-contains xml-output "<error"))))
