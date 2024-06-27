(define-module (2024-06-18-define-bug))

(use-modules (system base compile)
             (system vm loader))

(define (peval e)
  ((load-thunk-from-memory
    (compile
     e
     #:to 'bytecode
     #:env (resolve-module
            '(2024-06-18-define-bug))))))

(define (test-eval teval)
  (teval '(define a 1))
  (teval '(define a a))
  (format #t "a value with ~a is ~a\n" teval (teval 'a)))

(test-eval peval)
(test-eval primitive-eval)

;; (exit 0)

;; guile -l 2024-06-18-define-bug.scm # output is:
;; a value with #<procedure peval (e)> is #<unspecified>
;; a value with #<procedure primitive-eval (exp)> is 1
