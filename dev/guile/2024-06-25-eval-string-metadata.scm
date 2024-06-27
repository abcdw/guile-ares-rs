(define-module (2024-06-25-eval-string-metadata))

(use-modules (system vm program)
             (ice-9 eval-string))

(eval-string "(define (test-fn) 'hey)"
             #:module (current-module)
             #:file "hello.scm"
             #:line 1
             #:column 1
             #:compile? #f)

(format #t "~a\n" (program-sources test-fn))
;; ((0 ice-9/eval.scm 329 . 13) (12 ice-9/eval.scm 330 . 21) (44 ice-9/eval.scm 330 . 15))

(eval-string "(define (test-fn) 'hey)"
             #:module (current-module)
             #:file "hello.scm"
             #:line 1
             #:column 1
             #:compile? #t)

(format #t "~a\n" (program-sources test-fn))
;; ((0 hello.scm 1 . 1) (12 hello.scm 1 . 19))

(use-modules (system base language)
             (system vm program))
(call-with-input-string
 "(define (hello) 'hey)"
 (lambda (port)
   (set-port-filename! port "test.scm")
   (set-port-line! port 100)
   (set-port-column! port 0)

   (let ((reader (language-reader (lookup-language (current-language))))
         (eval (language-evaluator (lookup-language (current-language)))))
     (eval (pk (reader port (current-module))) (current-module)))))
(format #t "~a\n" (program-sources hello))
;;; (#<syntax:test.scm:101:0 (#<syntax:test.scm:101:1 define> #<syntax:test.scm:101:8 (#<syntax:test.scm:101:9 hello>)> #<syntax:test.scm:101:16 (quote #<syntax:test.scm:101:17 hey>)>)>)
;; ((0 ice-9/eval.scm 329 . 13) (12 ice-9/eval.scm 330 . 21) (44 ice-9/eval.scm 330 . 15))

(primitive-eval '(define (hello) 'hey))

(exit 0)
