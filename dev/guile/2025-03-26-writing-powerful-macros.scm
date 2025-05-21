(define-module (2025-03-26-writing-powerful-macros)
  #:use-module (rnrs lists)
  #:use-module (rnrs base))

(define-syntax incr!
  (syntax-rules ()
    ((t x)
     't)))

(incr! xy)

(define-syntax tmp-syntax
  (syntax-rules ()
    ((tmp-syntax (a ...) (b ...))
     '((a . b) ...))))

(tmp-syntax (1 2 3) (4 5 6))

(define-syntax trace-let
  (syntax-rules ()
    [(trace-let name ([var expr] ...) body1 ... body2)
     (let f ([depth 0] [var expr] ...)
       (define name
         (lambda (var ...)
           (f (+ depth 1) var ...)))
       (indent depth)
       (display "(")
       (display 'name)
       (begin
         (display " ")
         (display var))
       ...
       (display ")")
       (newline)
       (call-with-values
           (lambda ()
             body1 ... body2)
         (lambda val*
           (indent depth)
           (fold-left
            (lambda (sep val)
              (display sep)
              (display val)
              " ")
            "" val*)
           (newline)
           (apply values val*))))]))

;; Helper procedure referenced by the macro output of the macro above.
(define indent
  (let ([pattern "| "])
    (lambda (depth)
      (do ([i 0 (+ i 1)])
          ((> i depth))
        (display (string-ref pattern (mod i 2)))))))

(trace-let loop ((i 0))
           (if (< i  4)
               (loop (1+ i))
               'hi))

(define fact
  (lambda (n)
    (trace-let f ([n n])
      (if (zero? n)
          1
          (* (f (- n 1)) n)))))
;; (fact 3)

(define-syntax multi-incr!
  (syntax-rules ()
    ((multi-incr! x ...)
     (begin
       (set! x (1+ x))
       ...))))

(define a 3)
(define b 7)

(multi-incr! a b)


(let-syntax
    ((outer-x (identifier-syntax #'x)))
  (define inner-x #'x)
  (list (bound-identifier=? inner-x outer-x)
        (free-identifier=? inner-x outer-x))
  outer-x)

(let* ([x 1])
  (define i1 #'x)
  (let ((x 2))
    (define i2 #'x)
    (list (bound-identifier=? i1 i2)
          (free-identifier=? i1 i2))))

(syntax-case #'(1 2 3) ()
  [(1 x ...) #'(a x ... b c)])

(define-syntax define-vector-reference
  (syntax-rules ()
    [(define-vector-reference var vec-expr idx-expr)
     (begin
       (define vec vec-expr)
       (define idx idx-expr)
       (define-syntax var
         (identifier-syntax
          [var (vector-ref vec idx)]
          [(set! var expr) (vector-set! vec idx expr)])))]))

(define vec1 #(5 6 7 8))
(define-vector-reference initial-elem vec1 0)
(procedure-properties(make-variable-transformer (lambda (x) x)))

(let* ((x 1)
       (y x))
  (set! x 2)
  y)

;; identifier?
