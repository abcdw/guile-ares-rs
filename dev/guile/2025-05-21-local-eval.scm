(define-module (2025-05-21-local-eval)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 exceptions))

(use-modules (system syntax))
(use-modules (system vm frame)
             (system base compile))

(use-modules (srfi srfi-197)
             (ares guile prelude)
             (ice-9 match))


(define ds #f)
(define t* (make-parameter 17))

(define fn
  (let ((hehe 'test))
    (lambda (closure)
      (let ((a 134)
            (b (t*)))
        (throw (symbol-append hehe 'ho))
        (pk b a closure hehe)
        hehe))))

(define (add-bindings-to-module! module bindings)
  (for-each
   (lambda (b)
     (match b
       ((var . value)
        (module-define! module var value))))
   bindings)
  module)

(define (clear-module-bindings! module)
  "Doesn't really work"
  (hash-clear! (module-obarray module))
  (hash-clear! (module-obarray (module-public-interface module)))
  ;; TODO: [Andrew Tropin, 2025-07-03] Try to set some variable or
  ;; reload-module after clean.
  ;; TODO: [Andrew Tropin, 2025-07-03] Restore previous bindings
  )

(define (duplicate-module! module)
  (define new-module (make-fresh-user-module))
  (module-for-each
   (lambda (name var) (module-define! new-module name var))
   module)
  (set-module-name! new-module (module-name module))
  new-module)

(define (tmp-fn)
  (define env #f)
  (with-exception-handler
   (lambda (ex) ex)
   (lambda ()
     (with-exception-handler
      (lambda (ex)
        (pk
         (frame-environment (stack-ref (make-stack #t) 3)))
        (set! env
              (frame-environment (stack-ref (make-stack #t) 3))))
      (lambda ()
        (parameterize ((t* 20))
          (fn 'hello)))
      #:unwind? #f))
   #:unwind? #t)

  (pk fn)

  (format #t "~y" env)
  ;; (duplicate-module! (current-module))
  (define nested-eval-module
    (add-bindings-to-module! (current-module) env))
  ;; (module-use! (current-module) (module-public-interface nested-eval-module))
  ;; (hash-map->list (lambda (k v) (cons k v)) (module-obarray nested-eval-module))
  ;; (hash-map->list (lambda (k v) (cons k v)) (module-obarray (current-module)))
  ;; (module-map (lambda (t k) (cons t k)) (current-module))
  ;; (module-uses (current-module))
  ;; (format #t "a: ~a\n" a)
  ;; (define tmp-b (module-map (lambda (t k) (cons t k)) (current-module)))
  ;; (caddr tmp-b)

  ;; (clear-module-bindings! nested-eval-module)
  ;; (reload-module nested-eval-module)
  (format #t "compile: ~a\n" (compile '(+ a (t*)) #:env nested-eval-module))

  ;; (define tmp-module (make-fresh-user-module))
  ;; (set-module-name! tmp-module '(hello there))
  ;; tmp-module
  )


;;;
;;; Eval expressions in tmp-fn one by one
;;;
;;; Try to change (current-module) to (duplicate-module! (current-module))
;;;

;; (tmp-fn)
;; a
;; (module-uses (current-module))
;; (module-)






(comment
 ((record-accessor (record-rtd env) 'patterns) env)
 (define scp ((record-accessor (record-rtd env) 'scope) env))

 (syntax-locally-bound-identifiers ((record-accessor (record-rtd env) 'scope) env))

 (use-modules (srfi srfi-69))
 (hash-table->alist
  (module-obarray
   (module-public-interface m)))
 (use-modules (system base compile))
 (syntax-module scp)
 (datum->syntax scp '(hello a there))
 (record-type-field-names (record-rtd env))
 (fn 'hello)
 (with-exception-handler
  (lambda (ex)
    (set! env
          (frame-environment (stack-ref (make-stack #t) 3))))
  (lambda ()
    (parameterize ((t* 20))
      (fn 'hello)))
  #:unwind? #f)
 ((assoc-ref env 'closure) 'hei)




 ((chain (record-type-descriptor env)
    (record-accessor _ 'boxes))
  env)

 (with-dynamic-state ds
   (lambda ()
     (map
      (lambda (expression)
        (local-eval expression env))
      '((t*) a b))))


 (define eval-stack-prompt (make-prompt-tag "hey"))

 (define (co t)
   (pk t)
   (ho t))

 (define (ho t)
   (pk t)
   (make-stack #t 2 19))

 (define (hey)
   (call-with-prompt
    'hey
    (lambda ()
      (start-stack 'hey (co (stack-ref (make-stack #t) 0))))
    (lambda (c)
      (pk c))))

 (define (hm t)
   (define tmp (hey))
   (pk t)
   tmp)


 (define st (hm 'k))
 (display-backtrace st (current-output-port))
 )



;; Doesn't work yet
(define-syntax dbg
  (syntax-rules ()
    ((_ rest ...)
     (let ((e (the-environment)))
       (raise-exception
        (make-exception-with-irritants (list e)))))))

