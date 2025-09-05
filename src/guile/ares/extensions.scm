;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023, 2024, 2025 Andrew Tropin <andrew@trop.in>
;;;
;;; This file is part of guile-ares-rs.
;;;
;;; guile-ares-rs is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; guile-ares-rs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guile-ares-rs.  If not, see <http://www.gnu.org/licenses/>.

(define-module (ares extensions)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 match)
  #:use-module (ares exceptions)
  #:use-module (ares topological-sort)
  #:use-module (srfi srfi-1)
  #:export (make-handler
            extension?
            sort-extensions
            get-operations-directory))

;;;
;;; Ares Extension Mechanism for runtime modification of event loop.
;;;

(define (unknown-op context)
  "Always returns unknown-op error."
  (let ((reply!
         (or
          (assoc-ref context 'reply!)
          (assoc-ref context 'transport/reply!))))
    (reply! `(("nrepl/message" . ,(assoc-ref context 'nrepl/message))
              ("status" . #("error" "unknown-op" "done"))))
    context))

(define extension-metadata procedure-properties)

(define (extension? extension)
  "Checks that all necessary metainformation is provided and have a
correct type."
  (define metadata (extension-metadata extension))

  (define (raise-type-error metadata key type value)
    (raise-assert (format #f "\
In extensions ~s, key @code{~s} must be of type @code{~s}, but the value is \
@code{~s}"
                          (assoc-ref metadata 'name) key type value)))

  (define (check-type key type)
    (unless (type (assoc-ref metadata key))
      (raise-type-error
       metadata key (procedure-name type) (assoc-ref metadata key))))

  (define (list-of-symbols? lst)
    (and (list? lst) (every symbol? lst)))

  (unless (procedure? extension)
    (raise-assert (format #f "Extension ~s must be a function." extension)))

  (unless (assoc 'name metadata)
    (raise-assert (format #f "Extension ~s must have a name." extension)))

  (match (procedure-minimum-arity extension)
    ((1 _ _) 'ok)
    (_ (raise-assert
        (format #f "Extension ~s must be a function of 1 argument."
                (procedure-name extension)))))

  (check-type 'name symbol?)
  (check-type 'documentation string?)
  (check-type 'requires list-of-symbols?)
  (check-type 'provides list-of-symbols?))

(define (extensions->providers extensions)
  (define (raise-provided-more-than-once key v1 v2)
    (raise-assert
     (format #f "\
Key @code{~s} provided more than once, by both @code{~s} and @code{~s}."
             key v1 v2)))

  (define provides (make-hash-table)) ; what | who

  (for-each
   (lambda (x)
     (let ((ext-name (procedure-property x 'name)))
       (for-each
        (lambda (r)
          (when (hash-get-handle provides r)
            (raise-provided-more-than-once
             r (hash-ref provides r) ext-name))
          (hash-create-handle! provides r x))
        (procedure-property x 'provides))))
   extensions)
  provides)

(define (extensions->dependecy-graph extensions)
  "Build a directed graph (adjacency list) based on requires, provides
and wraps values.  If a has b in requires add a->b edge.  If a has b
in wraps add b->a edge.  If a wraps #:* then for every b not equal to
a and not required by a add b->a edge."
  (define provides (extensions->providers extensions))

  (define (raise-nobody-provides x for)
    (raise-assert
     (format #f "\
There are no nodes providing @code{~s}, but @code{~s} requires it" x for)))

  (define (who-provides x for)
    (unless (hash-get-handle provides x)
      (raise-nobody-provides x for))
    (procedure-property (hash-ref provides x) 'name))

  (define graph (make-hash-table))

  (define (update-hash-value! hash key f default-value)
    (let* ((handle (hash-get-handle hash key))
           (old-value (if handle (cdr handle) default-value)))
      (hash-set! hash key (f old-value))))

  (define (add-vertex! from to)
    (update-hash-value! graph from (lambda (x) (cons to x)) '()))

  (for-each
   (lambda (x)
     (let ((name (procedure-property x 'name)))
       (hash-set! graph name '())
       (for-each
        (lambda (y)
          (let ((provider (who-provides y name)))
            (unless (member provider (hash-ref graph name))
              (add-vertex! name provider))))
        (procedure-property x 'requires))))
   extensions)

  (define (provides? e c)
    (member c (procedure-property e 'provides)))

  (define (requires? x y)
    (fold
     (lambda (a res)
       (or res (provides? y a)))
     #f
     (procedure-property x 'requires)))

  ;; Add edges from wrapped nodes to wrappers
  (for-each
   (lambda (x)
     (let ((name (procedure-property x 'name))
           (wraps (procedure-property x 'wraps)))
       (cond
        ((equal? #:* wraps)
         (for-each
          (lambda (y)
            (let ((y-name (procedure-name y)))
              (unless (or (equal? x y) (requires? x y)
                          (member name (hash-ref graph y-name)))
                (add-vertex! y-name name))))
          extensions))
        ((list? wraps)
         (for-each
          (lambda (t)
            (unless (member t (hash-ref graph name))
              (add-vertex! t name)))
          wraps)))))
   extensions)
  graph)

(define (sort-extensions extensions)
  "Sorts extensions in topological order based on requires and provides
values."
  (for-each extension? extensions)

  (define provides (extensions->providers extensions))

  ;; Use fold instead of map to reverse the result.
  (fold
   (lambda (name result)
     (cons (hash-ref provides name) result))
   '()
   (topological-sort
    (hash-map->list cons (extensions->dependecy-graph extensions)))))

(define (print-sorted-graph extensions)
  (let ((graph (extensions->dependecy-graph extensions)))
    (map
     (lambda (k)
       (format #t "~a: ~a\n" k (hash-ref graph k)))
     (reverse (topological-sort
               (hash-map->list cons graph))))))

(define (print-sorted-extensions extensions)
  (for-each
   (lambda (e) (format #t "name: ~a\nprovides: ~a\nrequires: ~a\nwraps: ~a\n\n"
                       (procedure-property e 'name)
                       (procedure-property e 'provides)
                       (procedure-property e 'requires)
                       (or (procedure-property e 'wraps) '())))
   (sort-extensions extensions)))

(define (make-handler extensions)
  "Sorts the extensions using @code{sort-extensions}.  Wraps @code{unknown-op}
 into all the extensions in the reverse order."
  ;; (print-sorted-extensions extensions)
  ;; (print-sorted-graph extensions)
  (cons
   (fold (lambda (extension handler)
           (extension handler))
         unknown-op
         (reverse
          (sort-extensions extensions)))
   extensions))

(define (get-operations-directory extensions)
  "Return a list of operations provided by @code{extensions}."
  (define (get-operation-description operation)
    (match operation
      ((name . handler)
       (cons name (procedure-documentation handler)))))
  (define (get-extension-operations extension)
    (let ((handles (or (procedure-property extension 'handles) '())))
      (map get-operation-description handles)))
  (fold
   (lambda (x acc)
     (append (get-extension-operations x) acc))
   '()
   (sort-extensions extensions)))
