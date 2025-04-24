;; (ares topological-sort) -- topological sorting
;; Written 1995 by Mikael Durfeldt.

;; This file is based on tsort.scm from SLIB, and is in the public
;; domain. This file is from guile-lib. Only the define-module call
;; was changed to fit the project.

;;; Commentary:
;; The algorithm is inspired by Cormen, Leiserson and Rivest (1990)
;; ``Introduction to Algorithms'', chapter 23.
;;; Code:

(define-module (ares topological-sort)
  #:export (topological-sort)
  #:use-module (math primes))

(define (topological-sort-helper dag insert lookup)
  (if (null? dag)
      '()
      (let* ((adj-table (make-hash-table
			 (car (primes> (length dag) 1))))
	     (sorted '()))
	(letrec ((visit
		  (lambda (u adj-list)
		    ;; Color vertex u
		    (insert adj-table u 'colored)
		    ;; Visit uncolored vertices which u connects to
		    (for-each (lambda (v)
				(let ((val (lookup adj-table v)))
				  (if (not (eq? val 'colored))
				      (visit v (or val '())))))
			      adj-list)
		    ;; Since all vertices downstream u are visited
		    ;; by now, we can safely put u on the output list
		    (set! sorted (cons u sorted)))))
	  ;; Hash adjacency lists
	  (for-each (lambda (def)
		      (insert adj-table (car def) (cdr def)))
		    (cdr dag))
	  ;; Visit vertices
	  (visit (caar dag) (cdar dag))
	  (for-each (lambda (def)
		      (let ((val (lookup adj-table (car def))))
			(if (not (eq? val 'colored))
			    (visit (car def) (cdr def)))))
		    (cdr dag)))
	sorted)))

(define (topological-sort dag)
"Returns a list of the objects in the directed acyclic graph, @var{dag}, topologically sorted.  Objects are
compared using @code{equal?}.  The graph has the form:
@lisp
 (list (obj1 . (dependents-of-obj1)) 
       (obj2 . (dependents-of-obj2)) ...)
@end lisp
...specifying, for example, that @code{obj1} must come before all the objects in @code{(dependents-of-obj1)} in
the sort."
  (topological-sort-helper dag hash-set! hash-ref))

(define (topological-sortq dag)
"Returns a list of the objects in the directed acyclic graph, @var{dag}, topologically sorted.  Objects are
compared using @code{eq?}.  The graph has the form:
@lisp
 (list (obj1 . (dependents-of-obj1)) 
       (obj2 . (dependents-of-obj2)) ...)
@end lisp
...specifying, for example, that @code{obj1} must come before all the objects in @code{(dependents-of-obj1)} in
the sort."
  (topological-sort-helper dag hashq-set! hashq-ref))

(define (topological-sortv dag)
"Returns a list of the objects in the directed acyclic graph, @var{dag}, topologically sorted.  Objects are
compared using @code{eqv?}.  The graph has the form:
@lisp
 (list (obj1 . (dependents-of-obj1)) 
       (obj2 . (dependents-of-obj2)) ...)
@end lisp
...specifying, for example, that @code{obj1} must come before all the objects in @code{(dependents-of-obj1)} in
the sort."
  (topological-sort-helper dag hashv-set! hashv-ref))

;;; arch-tag: 9ef30b53-688a-43fc-b208-df78d5b38c74
