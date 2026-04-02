(define-module (ares-demo 02-stack-traces-test)
  #:use-module (ares suitbl core)
  #:use-module (ares guile prelude)
  #:use-module (ares-demo 02-stack-traces))

(define (fine-test)
  (test "fine"
    (is (even? 4))))

(define-suite kek
  'metadata
  '((demo? . #t))

  (test "another fine"
    ;; (sleep 1)
    (is 'ok))

  (test "7 div 2 = 3"
    (is (= (some-fn 7) 3)))

  (test "8 div 2 = 4"
    (is (= (some-fn 8) 4)))

  (fine-test))
