;; https://trop.in/blog/actually-useful-stack-traces

(define-module (ares-demo 02-stack-traces))

(define (div2-error x)
  (let ((result (floor (/ x 0))))
    (+ 3 x)
    result))

(define (div2-wrong x)
  (let ((result (/ x 2)))
    (+ 3 x)
    result))

(define (div2-correct x)
  (let ((result (floor (/ x 2))))
    (+ 3 x)
    result))

;; (map tmp (list 1 2 3))

(define-public (some-fn x)
  (let ((result (div2-correct x)))
    (+ 3 x)
    result))

(when #f
  'something
  'not-something)

;; (some-fn 5)

;; (+ 2 2)

;; (map 1+ (list 1 2 3))
;; (values 1 2 3)
