;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023, 2024 Andrew Tropin <andrew@trop.in>
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
  #:use-module (ares exceptions)
  #:use-module (srfi srfi-1)
  #:export (make-handler
            sort-extensions))

;;;
;;; Ares Extension Mechanism for runtime modification of event loop.
;;;

(define (unknown-op context)
  (let ((reply!
         (or
          (assoc-ref context 'reply!)
          (assoc-ref context 'transport/reply!))))
    (reply! `(("status" . #("error" "unknown-op" "done"))))
    context))

(define (extension? ext)
  (define (raise-type-error ext key type)
    (raise-assert (format #f "\
In extensions ~s, key @code{~s} must be of type @code{~s}."
                          (assoc-ref ext 'name) key type)))

  (define (check-type key type)
    (unless (type (assoc-ref ext key))
      (raise-type-error ext key (procedure-name type))))

  (unless (list? ext)
    (raise-assert (format #f "Extension ~s must be an alist." ext)))

  (unless (assoc 'name ext)
    (raise-assert (format #f "Extensions ~s must have a name." ext)))
  (check-type 'name string?)
  (check-type 'requires list?))

(define (sort-extensions extensions)
  (for-each extension? extensions)

  (define (raise-provided-more-than-once key v1 v2)
    (raise-assert
     (format #f "\
Key @code{~s} provided more than once, by both @code{~s} and @code{~s}."
               key v1 v2)))

  (define (raise-nobody-provides x for)
    (raise-assert
     (format #f "\
There are no nodes providing @code{~s}, but @code{~s} requires it" x for)))

  (define provides (make-hash-table)) ; what | who

  (for-each
   (lambda (x)
     (let ((ext-name (assoc-ref x 'name)))
       (for-each
        (lambda (r)
          (when (hash-get-handle provides r)
            (raise-provided-more-than-once
             r (hash-ref provides r) ext-name))
          (hash-create-handle! provides r ext-name))
        (assoc-ref x 'provides))))
   extensions)

  (define (who-provides x for)
    (unless (hash-get-handle provides x)
      (raise-nobody-provides x for))
    (hash-ref provides x))

  (define graph (make-hash-table))

  (define (update-hash-value! hash key f default-value)
    (let* ((handle (hash-get-handle hash key))
           (old-value (if handle (cdr handle) default-value)))
      (hash-set! hash key (f old-value))))

  (define (add-vertex! from to)
    (update-hash-value! graph from (lambda (x) (cons to x)) '()))

  (for-each
   (lambda (x)
     (let ((name (assoc-ref x 'name)))
       (hash-set! graph name '())
       (for-each
        (lambda (y)
          (let ((provider (who-provides y name)))
            (unless (member provider (hash-ref graph name))
              (add-vertex! name provider))))
        (assoc-ref x 'requires))))
   extensions)

  (define (hash->list hash)
    (hash-map->list cons hash))

  (define (less ext1 ext2)
    (member
     (assoc-ref ext1 'name)
     (hash-ref graph (assoc-ref ext2 'name))))

  (stable-sort extensions less))

(define (make-handler extensions)
  (cons
   (fold (lambda (extension handler)
           ((assoc-ref extension 'wrap) handler))
         unknown-op
         (reverse
          (sort-extensions extensions)))
   extensions))
