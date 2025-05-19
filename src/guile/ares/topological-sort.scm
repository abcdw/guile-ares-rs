(define-module (ares topological-sort)
  #:use-module (srfi srfi-1)
  #:export (topological-sort))

;;; toposort.scm - topological sorting
;;;
;;;  Written by Shiro Kawai (shiro@acm.org)  2001
;;;  Public Domain..  I guess lots of Scheme programmers have already
;;;  written similar code.
;;;
;;;  Modified 2025 to make EQ default to equal?

(define* (topological-sort nodes #:optional (eq equal?))
  (define table (map (lambda (n) (cons (car n) 0)) nodes))
  (define queue '())
  (define result '())

  (define (set-up)
    ;; Compute the number of nodes that each node depends on.
    (for-each
      (lambda (node)
        (for-each
          (lambda (to)
            (let ((p (assoc to table eq)))
              (if p
                  (set-cdr! p (+ 1 (cdr p)))
                  (set! table (cons (cons to 1) table)))))
          (cdr node)))
      nodes))

  (define (traverse)
    (unless (null? queue)
      (let ((nq (car queue)))
        (set! queue (cdr queue))
        (let ((n0 (assoc nq nodes eq)))
          (when n0
            (for-each
              (lambda (to)
                (let ((p (assoc to table eq)))
                  (when p
                    (let ((cnt (- (cdr p) 1)))
                      (when (zero? cnt)
                        (set! result (cons to result))
                        (set! queue (cons to queue)))
                      (set-cdr! p cnt)))))
              (cdr n0)))
          (traverse)))))

  (set-up)
  (set! queue (map car (filter (lambda (p) (zero? (cdr p))) table)))
  (set! result queue)
  (traverse)
  (let ((rest (filter (lambda (e) (not (zero? (cdr e)))) table)))
    (unless (null? rest)
      (error "Graph has circular dependency" (map car rest))))
  (reverse result))
