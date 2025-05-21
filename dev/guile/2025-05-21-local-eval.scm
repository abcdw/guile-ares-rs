(define-module (2025-05-21-local-eval)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 exceptions))


(define env #f)
(define ds #f)
(define t* (make-parameter 17))

(define (fn)
  (let ((a 134)
        (b (t*)))
    (set! env (the-environment))
    (set! a 144)
    (set! ds (current-dynamic-state))))

(parameterize ((t* 20))
  (fn))

(use-modules (srfi srfi-197))
((chain (record-type-descriptor env)
        (record-accessor _ 'boxes))
 env)

(with-dynamic-state ds
  (lambda ()
    (map
     (lambda (expression)
       (local-eval expression env))
     '((t*) a b))))



;; Doesn't work yet
(define-syntax dbg
  (syntax-rules ()
    ((_ rest ...)
     (let ((e (the-environment)))
       (raise-exception
        (make-exception-with-irritants (list e)))))))
