;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2025 Libre en Communs <contact@a-lec.org>
;;; Copyright © 2025 Noé Lopez <noelopez@free.fr>
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

(define-module (ares topological-sort-test)
  #:use-module (ares topological-sort)
  #:use-module (srfi srfi-64)
  #:use-module (test-utils))

(define test-graph
  '((banana mandarin)
    (apple banana pear)
    (mandarin raspberry)
    (pear banana)
    (raspberry)))

(define-test test-topological-sort
  (test-group
   "Topological sort"
   (test-equal '(apple pear banana mandarin raspberry) (topological-sort test-graph))
   (test-equal '(apple pear banana mandarin raspberry) (topological-sort (reverse test-graph)))
   (test-equal '() (topological-sort '()))
   (test-equal #f (topological-sort '(bazanga)))
   (test-equal #f (topological-sort '(a b c)))
   (test-equal '(a b c) (topological-sort '((a b) (b c) (c))))))
