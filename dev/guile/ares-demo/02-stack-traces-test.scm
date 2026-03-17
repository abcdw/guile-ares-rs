(define-module (ares-demo 02-stack-traces-test)
  #:use-module (ares suitbl core)
  #:use-module (ares guile prelude)
  #:use-module (ares-demo 02-stack-traces))

(comment
 (define-suite kek
   (test "7 div 2 = 3"
     (is (= (some-fn 7) 3)))
   (test "8 div 2 = 4"
     (is (= (some-fn 8) 4)))))
