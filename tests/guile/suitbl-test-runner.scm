(define-module (suitbl-test-runner)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl runners)
  #:use-module ((ares suitbl reporters) #:prefix reporter:)
  #:use-module (ares suitbl discovery)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-197)
  #:use-module (ice-9 exceptions)
  #:export (run-project-tests))

(define (suitbl-module? m)
  (let ((name (module-name m)))
    (and (>= (length name) 2)
         (eq? (first name) 'ares)
         (string-prefix? "suitbl" (symbol->string (second name))))))

(define (only-suitbl-tests tests state)
  (filter (lambda (t)
            (any (lambda (s)
                   (chain-and s
                     (assoc-ref _ 'suite/metadata)
                     (assoc-ref _ 'module)
                     (suitbl-module? _)))
                 (or (assoc-ref t 'suite/path) '())))
          tests))

(define-public (run-project-tests)
  (let* ((test-runner (make-suitbl-test-runner)))
    (parameterize ((test-runner* test-runner))
      ((@ (ares suitbl ares) load-project-tests))
      (test-runner `((type . runner/run-tests)
                     (runner/config
                      . ((schedule-tests . ,only-suitbl-tests))))))
    (define summary (test-runner `((type . runner/get-run-summary))))

    (define number-of-tests
      (assoc-ref summary 'tests))

    (unless (= 66 number-of-tests)
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
                       `((test-reporter . ,reporter:junit)))))
    (parameterize ((test-runner* test-runner))
      ((@ (ares suitbl ares) load-project-tests))
      (test-runner `((type . runner/run-tests))))))
