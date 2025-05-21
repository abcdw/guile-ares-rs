(define-module (2025-02-24-fear-of-macros-chapter-5))
(use-modules (ice-9 exceptions))

(define-syntax-rule (aif condition true-expr false-expr)
    (let ([it condition])
      (if it
          true-expr
          false-expr)))

(aif #t (display it) (void))

(define current-foo (make-parameter "some default value"))

(current-foo)

(parameterize ([current-foo "I have a new value, for now"])
  (current-foo))

(define-syntax-parameter it
  (lambda (stx)
    (raise-exception
     (make-exception
      (make-exception-with-message "can only be used inside aif")
      (make-syntax-error stx #f)))))

it

(define-syntax-rule (aif condition true-expr false-expr)
  (let ([tmp condition])
    (if tmp
        (syntax-parameterize ([it (syntax-case tmp () [_ #'tmp])])
                             true-expr)
        false-expr)))




(define-syntax-parameter abort (syntax-rules ()))
(define-syntax forever
  (syntax-rules ()
    [(forever body ...)
     (call/cc (lambda (abort-k)
                (syntax-parameterize
                 ([abort (syntax-rules () [(_) (abort-k)])])
                 (let loop () body ... (loop)))))]))

(forever
 (display "hi\n") (sleep 1) (abort))

(define hey 'hi)

(define tmp-fn
  (make-variable-transformer
   (lambda (x)
     (display (identifier? x))
     (display x)
     (newline)
     (syntax-case x (set!)
       (var (identifier? #'var) #'hey)
       ((set! var val) #'(set! hey val))
       ;; ((_ a1 a2) #'(hey a1 a2))
       ))))

;; (procedure-properties tmp-fn)

(define-syntax hey-alias tmp-fn)
(hey-alias 'here 'two)

(define first (lambda (x y) x))

(another-call
 (first hey-alias 'hohe)
 'another-argument)


(display hey-alias)
(set! hey-alias first)

((identifier-syntax (+ 1 2)) #'(a b)) ;; -> ((+ 1 2) b)

((identifier-syntax t) #'(whetever it happens (he ho)))

(define-syntax ist
  (identifier-syntax (i1 (+ 1 2 'i1))
                     ((set! vr vl) (- vl vr))))

(+ ist 5)

(define-syntax ist-alias
  (lambda (x)
    (syntax-case x ()
      (var (identifier? #'var) #'ist))))

(first ist 5)
(set! ist 5)

(define-syntax-parameter it
  (lambda (x)
    (syntax-case x ()
      (_ #'(syntax-error "it can't be used outside of aif")))))

(define-syntax aif
  (syntax-rules ()
    [(aif test then else)
     (let ([t test])
       (syntax-parameterize ([it (identifier-syntax t)])
                            (if t then else)))]))

(define-syntax aif
  (lambda (x)
    (syntax-case x ()
      [(aif test then else)
       #'(let ([t test])
           (syntax-parameterize ([it (identifier-syntax t)])
                                (if t then else)))])))

;; (+ it 'args)
;; (use-modules (ice-9 format))

(define tmp
  (lambda ()
    (let ((t 123))
      (aif (+ 3 4 t) (begin
                       (display t)
                       (newline)
                       (display it)) (void)))))

;; (macroexpand)

;; (decompile (compile '(aif #t (display it) (void)) #:to 'tree-il))
(use-modules (system base compile))
(decompile (compile '(when #t #f #f) #:to 'tree-il))



;; TODO: [Andrew Tropin, 2025-04-03] Send patch removing unecessary
;; meta-information from identifier-syntax.
