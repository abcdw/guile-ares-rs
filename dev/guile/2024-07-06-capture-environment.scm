(define-module (2024-07-06-capture-environment))

(use-modules (ice-9 local-eval))

(define kont #f)
(define b 34)

(define (cap-env)
  (the-environment))

(define-public captured-env #f)
(define-syntax test-equal
    (syntax-rules ()
      ((_ e1 e2)
       (begin
         (set! last-stack (make-stack #t))
         (set! captured-env (the-environment))
         (display 'hi)))))

(define last-stack #f)
(define-public tmp-env
  (let ((a 10))
    (let ((t 20))

      (test-equal 1 2)
      'hey
      (the-environment))))
(use-modules (system vm frame))


(use-modules (ice-9 format))
(for-each
 (lambda (binding)
   (let ((v (binding-ref binding)))
     (display "  ")
     ;; (run-hook before-print-hook v)
     (format #t "~a = ~v:@y\n" (binding-name binding) 80 v)))
 )

(frame-environment
  (stack-ref last-stack 1))

(let* ((a 10)
       (t 20)
       (env (the-environment)))
  (define exp (call/cc (lambda (k) (set! kont k) 'a)))
  (local-eval exp env))
(kont '(+ a b))
;; (display (local-eval '(+ 1 a) tmp-env))

;; (use-modules (ares srfi-64 test-runners)
;;              (srfi srfi-64))
;; (define (capture-test)
;;   (let ((a 130))
;;     (test-equal 1 2)))
;; (run-test capture-test)

;; (use-modules (language tree-il))
;; (tree-il->scheme (macroexpand '(test-equal 1 2)))
;; (local-eval '(+ a 20) (@ (srfi srfi-64) last-assert-env))

;; (hash-map->list
;;  (lambda (x y) x)
;;  (module-obarray ((@@ (ice-9 local-eval) env-module) tmp-env)))

;; ((car ((@@ (ice-9 local-eval) env-boxes) last-assert-env)))


