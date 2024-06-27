;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (ares guile)
  #:use-module (ice-9 match)
  #:export (define-with-meta))

;; The differences with usual define are: it always expects docstring
;; and meta, and meta is arbitrary expression returning alist.  The
;; usual define expects vector literal nad you can't unquote inside it
;; and acces the external environment.
(define-syntax define-with-meta
  (syntax-rules ()
    ((_ (name . params) docstring meta b0 b1 ...)
     (begin
       (define name (lambda params docstring b0 b1 ...))
       (for-each
        (lambda (x)
          (match x
            ((k . v)
             (set-procedure-property! name k v))))
        meta)))))
