;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

;;; function-inlining.scm --- Guile function inlining experiments

(define-module (guile-experiments function-inlining)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module ((srfi srfi-1) #:select (iota))
  #:use-module (language scheme compile-tree-il)
  #:use-module (language scheme decompile-tree-il)
  #:use-module (language tree-il optimize)
  #:use-module (system base compile)
  #:use-module (system vm debug)
  #:use-module (system vm disassembler)
  #:use-module (system vm frame)
  #:export (optimization-level
            plain-helper-expression
            mutable-helper-expression
            plain-stack-expression
            mutable-stack-expression
            optimized-form
            compiled-procedure
            stack-frames
            show-optimized-forms
            show-mutable-disassembly
            show-stack-comparison
            run-all))



(define optimization-level 2)

(define plain-helper-expression
  '(lambda ()
     (define (helper) 1)
     (helper)))

(define mutable-helper-expression
  '(lambda ()
     (define helper #f)
     (set! helper (lambda () 1))
     (helper)))

(define plain-stack-expression
  '(lambda ()
     (define (helper)
       (error "boom"))
     ;; Keep the call out of tail position so the caller frame can
     ;; survive tail call optimization.
     (list (helper))))

(define mutable-stack-expression
  '(lambda ()
     (define helper #f)
     (set! helper
           (lambda ()
             (error "boom")))
     ;; Keep the call out of tail position so the caller frame can
     ;; survive tail call optimization.
     (list (helper))))


(define (optimized-form expr)
  (let ((env (make-fresh-user-module)))
    (call-with-values
        (lambda ()
          (compile-tree-il expr env '()))
      (lambda (exp env cenv)
        (decompile-tree-il
         ((make-lowerer optimization-level '()) exp env)
         env
         '())))))

(define (compiled-procedure expr)
  (compile expr
           #:to 'value
           #:optimization-level optimization-level))

(define (frame-record frame)
  (let* ((ip (frame-instruction-pointer frame))
         (src (find-source-for-addr ip)))
    `((name . ,(frame-procedure-name frame))
      (file . ,(and src (source-file src)))
      (line . ,(and src (source-line-for-user src)))
      (column . ,(and src (source-column src))))))

(define (stack-frames expr)
  (let ((procedure (compiled-procedure expr)))
    (let/ec cancel
      (start-stack 'function-inlining
        (with-exception-handler
          (lambda (exn)
            (let ((stack (make-stack #t 1)))
              (cancel
               (map (lambda (index)
                      (frame-record (stack-ref stack index)))
                    (iota (stack-length stack))))))
          procedure))
      '())))

(define (show-frame-record index frame)
  (format #t "~2d: name=~s src=~a:~a:~a~%"
          index
          (assq-ref frame 'name)
          (or (assq-ref frame 'file) '#f)
          (or (assq-ref frame 'line) '#f)
          (or (assq-ref frame 'column) '#f)))


(define (show-optimized-forms)
  (format #t "Comparing optimized forms at -O~a~%~%"
          optimization-level)

  (format #t "Plain local define:~%")
  (pretty-print (optimized-form plain-helper-expression))
  (newline)

  (format #t "Mutable local binding:~%")
  (pretty-print (optimized-form mutable-helper-expression))
  (newline))

(define (show-mutable-disassembly)
  (format #t "Disassembly for mutable local binding at -O~a~%~%"
          optimization-level)
  (disassemble-program (compiled-procedure mutable-helper-expression))
  (newline))

(define (show-stack-comparison)
  (define (show-one label expr)
    (format #t "~a~%" label)
    (let loop ((frames (stack-frames expr))
               (index 0))
      (match frames
        (() (newline))
        ((frame . rest)
         (show-frame-record index frame)
         (loop rest (1+ index))))))

  (format #t "Stack comparison at -O~a~%"
          optimization-level)
  (format #t "The helper call is wrapped in (list (helper)) so it is not in tail position.~%")
  (format #t "Look for a frame named helper only in the mutable-binding case.~%~%")
  (show-one "Plain local define:" plain-stack-expression)
  (show-one "Mutable local binding:" mutable-stack-expression))

(define (run-all)
  (show-optimized-forms)
  (show-mutable-disassembly)
  (show-stack-comparison))
