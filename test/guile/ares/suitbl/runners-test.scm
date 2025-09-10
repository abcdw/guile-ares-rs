(define-module (ares suitbl runners-test)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl runners)
  #:use-module (ares suitbl reporters))

(define-syntax with-test-runner
  (lambda (stx)
    (syntax-case stx ()
      ((_ test-runner body body* ...)
       #'(parameterize ((test-runner* test-runner))
           body body* ...)))))

(define (silent-runner)
  (make-suitbl-test-runner #:test-reporter test-reporter-silent))

(define-suite assertions-handling-tests
  (test "is assert returns the value of its body"
    (define tr (silent-runner))
    (define is-values
      (with-test-runner tr
        (define b 'heyhey)
        (list
         (is #t)
         (is 123)
         (is 'some-symbol)

         (let ((a 123))
           (is a))
         (is b)

         (is (+ 2 3))
         (is (= 2 3)))))

    (is (equal?
         '(#t 123 some-symbol 123 heyhey 5 #f)
         is-values))))
