(define-module (suitbl-test-runner)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl runners)
  #:use-module ((ares suitbl reporters) #:prefix reporters:)
  #:use-module ((ares suitbl runner-state) #:prefix state:)
  #:use-module (ares suitbl discovery)
  #:use-module (srfi srfi-197)
  #:use-module (ice-9 exceptions)
  #:export (run-project-tests))

(define-public (run-project-tests)
  (let* ((test-runner (make-suitbl-test-runner)))
    (parameterize ((test-runner* test-runner))
      ((@ (ares suitbl ares) load-project-tests))
      (test-runner `((type . runner/run-tests))))
    (define summary (test-runner `((type . runner/get-run-summary))))
    (format #t "\n~a\n" summary)

    (define number-of-tests
      (assoc-ref summary 'tests))

    (unless (= 31 number-of-tests)
      (chain "Unexpected number of tests (~a), make sure all tests are executed and
expected number of tests is up-to-date."
        (format #f _ number-of-tests)
        (make-exception-with-message _)
        (raise-exception _)))
    (when (> (+ (assoc-ref summary 'failures) (assoc-ref summary 'errors)) 0)
      (exit 1))))

(define-public (run-project-tests-junit-output)
  (let* ((test-runner (make-suitbl-test-runner
                       #:config
                       `((test-reporter . ,reporters:test-reporter-silent)))))
    (define junit-xml
      (parameterize ((test-runner* test-runner))
        ((@ (ares suitbl ares) load-project-tests))
        (test-runner `((type . runner/run-tests)))
        (chain (test-runner `((type . runner/get-state)))
          (state:get-suite-forest-with-summary _)
          (reporters:forest->junit-xml _))))
    (format #t "~a" junit-xml)))
