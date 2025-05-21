(define-module (2025-03-24-syntax-rules-pdf))

(define-syntax emit-cwv-form
  (syntax-rules ()
    ((emit-cwv-form temps assignments values-form)
     (call-with-values (lambda () values-form) (lambda temps . assignments)))))

(define-syntax gen-temps-and-sets
  (syntax-rules ()
    ((gen-temps-and-sets () temps assignments values-form)
     (emit-cwv-form temps assignments values-form))
    ((gen-temps-and-sets (variable . more) temps assignments values-form)
     (gen-temps-and-sets
      more
      (temp . temps)
      ((set! variable temp) . assignments) values-form))))

(define-syntax multiple-value-set!
  (syntax-rules ()
    ((multiple-value-set! variables values-form)
     (gen-temps-and-sets
      variables
      () ;; initial value of temps
      () ;; initial value of assignments
      values-form))))

(define a)
(define b)
(define c)

(multiple-value-set! (a b c) (values 1 2 3))
