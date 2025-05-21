(define-module (2025-02-17-fear-of-macros))

(define-syntax foo
  (lambda (stx)
    (syntax "I am foo")))

;; TODO: [Andrew Tropin, 2025-02-17] propose lambda-less function
;; (define-syntax (foo stx) ...) to Guile

(foo "test" (+ 1 2) (display "this") "this")

(syntax (fn "first" "I am foo"))

#'(fn "first" "I am foo")

(define (displayln something)
  (display something)
  (newline))

;; (eval-when 'expand)

(define-syntax foo
  (lambda (stx)
    (displayln stx)
    (syntax "I am foo")))

;; (syntax- )
(foo '(+ 1 2))
(cdr (syntax->datum #'(fn "first" "I am foo")))
;; (source-properties )

(use-modules (system syntax))
;; (syntax-sourcev)


(define-syntax reverse-me
  (lambda (stx)
    (datum->syntax stx (reverse (cdr (syntax->datum stx))))))

(let ((a "am"))
  (reverse-me "backwards" a "i" values))

(datum->syntax #f
               (reverse (cdr (syntax->datum #'(reverse-me "backwards" "am" "i" values)))))

(define-syntax our-if
  (lambda (x)
    (syntax-case x ()
      ((_ c e1 e2)
       (syntax (cond (c e1)
                     (else e2)))))))

(let ((a "hello"))
  (our-if #f (begin (display "hi") a) 2))
