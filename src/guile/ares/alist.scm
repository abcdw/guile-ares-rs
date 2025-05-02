;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023, 2025 Andrew Tropin <andrew@trop.in>
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

(define-module (ares alist)
  #:use-module (srfi srfi-1)
  #:export (alist-get-in alist-select-keys)
  #:re-export (alist-cons
               alist-delete))

(define (alist-get-in path alist)
  (if (null? path)
      alist
      (alist-get-in (cdr path) (assoc-ref alist (car path)))))

(define (alist-select-keys keys alist)
  "Select entries with keys equal to @code{keys} and return a new alist
with those entries. The order of entries corresponds to order of @code{keys}."
  (fold-right
   (lambda (key acc)
     (let ((entry (assoc key alist)))
       (if entry
           (cons entry acc)
           acc)))
   '()
   keys))
