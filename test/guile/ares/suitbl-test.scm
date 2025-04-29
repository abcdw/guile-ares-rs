(define-module (ares suitbl-test)
  #:use-module (ares guile prelude)
  #:use-module (ares suitbl)
  #:use-module (ice-9 exceptions))



(define (get-simple-test-runner)
  (define (simple-test-runner message)
    "Very simple test runner, just returns the result of @code{is} assert."
    (define (simple-run-assert form-thunk args-thunk quoted-form)
      (with-exception-handler
       (lambda (ex)
         'error)
       (lambda ()
         (let* ((result (form-thunk)))
           (if result 'pass 'fail)))
       #:unwind? #t))

    (let ((msg-type (assoc-ref message 'type)))
      (case msg-type
        ((schedule-test-case)
         (let ((test-case-thunk (assoc-ref message 'test-case-thunk)))

           (test-case-thunk)))

        ((run-assert)
         (let ((assert-thunk (assoc-ref message 'assert-thunk))
               (assert-quoted-form (assoc-ref message 'assert-quoted-form)))
           (simple-run-assert assert-thunk #f assert-quoted-form)))

        (else
         (raise-exception
          (make-exception-with-message
           (format #f "~a handling is not implemented by simple test runner"
                   (assoc-ref message 'type))))))))
  simple-test-runner)

(define-syntax ignore-current-output-port
  (lambda (stx)
    (syntax-case stx ()
      ((_ body body* ...)
       #'(parameterize ((test-runner-output-port* (open-output-string)))
           body body* ...)))))

(reset-test-environment
 get-simple-test-runner
 (is
  (ignore-current-output-port
   (reset-test-environment
    (@@ (ares suitbl) default-get-test-runner)
    (test-case "test"
      (is 123))))))



;;;
;;; Tests for is, test-case, test-suite that we can use to test test runners
;;;

(comment
 (display 'hi)
 (+ 1 2)
 'hi)

(define-test-suite is-usage-test-suite
  (test-case "basic atomic values"
    (is #t)
    (is 123)
    (is 'some-symbol))

  (test-case "an expression asserting atmoic value of the variable"
    (let ((a 123))
      (is a)))

  (test-case "predicates"
    (is (= 1 1))
    (is (even? 14)))

  (test-case "nested is and is return value"
    (is (= 7 (is (+ 3 4)))))

  ;; Do we need xfail/xpass?
  ;; We can implement it also with test-case metadata and custom test runner
  (test-case "expected to fail"
    ;; Remove not, when it implemented correctly (starts to pass)
    (is (not now-it-false-but-later-will-become-true))))

(define-test-suite test-case-usage-test
  (test-case "simple test case with meta"
    #:metadata `((expected-to-fail . #t))
    (is #f)
    (is #t)))

(define-test-suite test-runner-test-suite
  (is-usage-test-suite))

(define-test-suite execution-timeout-test-suite
  ;; https://legacy.cs.indiana.edu/~dyb/pubs/engines.pdf
  (is #f))

(define (run-tests)
  (test-runner-test-suite))
