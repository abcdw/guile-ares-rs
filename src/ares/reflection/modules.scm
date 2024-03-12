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

(define-module (ares reflection modules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:export (all-modules
            module-filename
            string->resolved-module))

(define (submodules mod)
  "Return a list of submodules."
  (hash-map->list (lambda (k v) v) (module-submodules mod)))

(define (root-modules)
  "Return a list of submodules of '() module."
  (submodules (resolve-module '() #f)))

(define* (all-child-modules mod #:optional (seen '()))
  "Recursively traverse all the submodules of MOD and build a list out of
it."
  (let ((mod-submodules
         (filter (lambda (m) (not (member m seen))) (submodules mod))))
    (fold (lambda (m all) (append (all-child-modules m all) all))
          (list mod)
          mod-submodules)))

(define (all-modules)
  "Return a list of all modules."
  (resolve-module '(boot-9))
  (let* ((roots (root-modules))
         (children (append-map all-child-modules roots)))
    children))

(define (modules-with-module-kind)
  "Return a list of all modules, having a module-kind."
  (define (module-with-module-kind m)
    (and (module-kind m) m))
  (let* ((guile-module (resolve-module '(guile)))
         (roots (root-modules))
         (children (append-map all-child-modules roots)))
    (filter-map module-with-module-kind children)))

(define (module-filename mod)
  "Return a path to module if corresponding file found in %load-path."
  (let ((name-parts
         (cond ((equal? '(boot-9) (module-name mod)) '("ice-9" "boot-9"))
               ((equal? '(guile) (module-name mod)) '("ice-9" "boot-9"))
               (else (map symbol->string (module-name mod))))))
    (and=>
     (%search-load-path (string-join name-parts "/"))
     canonicalize-path)))

(define (string->resolved-module str)
  "Tries to resolve STR to a module object."
  (and-let* ((module-name (with-input-from-string str read))
             (_ (pair? module-name)))
    (resolve-module module-name #:ensure #f)))
