;; SPDX-License-Identifier: MIT
;; SPDX-FileCopyrightText: 2021 Marc Nieper-Wißkirchen
;; Source: https://srfi.schemers.org/srfi-229/

(define-library (srfi 229)
  (export case-lambda/tag
	  lambda/tag
	  procedure/tag?
	  procedure-tag)
  (import (scheme base)
	  (scheme case-lambda))
  (begin
    (define *tagged-procedures* '())

    (define key (list 'key))

    (define make-procedure/tag
      (lambda (tag proc)
	(define f
	  (case-lambda
	    ((arg)
	     (if (eq? arg key) tag (proc arg)))
	    (arg*
	     (apply proc arg*))))
	(set! *tagged-procedures* (cons f *tagged-procedures*))
	f))

    (define-syntax case-lambda/tag
      (syntax-rules ()
	((case-lambda/tag expr (formals body1 ... body2) ...)
	 (make-procedure/tag
	  expr
	  (case-lambda (formals body1 ... body2) ...)))))

    (define-syntax lambda/tag
      (syntax-rules ()
	((lambda/tag expr formals body1 ... body2)
	 (make-procedure/tag
	  expr
	  (lambda formals body1 ... body2)))))

    (define procedure/tag?
      (lambda (f)
	(and (memv f *tagged-procedures*) #t)))

    (define procedure-tag
      (lambda (f)
	(f key)))))
