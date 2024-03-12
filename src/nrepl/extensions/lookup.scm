;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2024 Nikita Domnitskii
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

(define-module (nrepl extensions lookup)
  #:use-module (ares reflection modules)
  #:use-module ((ice-9 regex) #:select (regexp-quote))
  #:use-module ((ice-9 session) #:select (apropos-fold
                                          apropos-fold-accessible))
  #:use-module (srfi srfi-197)
  #:use-module (srfi srfi-2)
  #:use-module (system vm program)
  #:export (lookup-extension))

(define (lookup-symbol ns sym)
  (apropos-fold
   ;; NOTE: [Nikita Domnitskii, 2024-03-12] Not sure how this would
   ;; affect perfomance, maybe we should add some form of early return?
   (lambda (module name var init)
     (if (eq? sym name)
         (and-let* ((src (cond
                          ((macro? var) (chain-and
                                         (macro-transformer var)
                                         (program-source _ 0)))
                          ((program? var) (program-source var 0))
                          (else #f))))
           `(("file" . ,(chain
                         (source:file src)
                         (%search-load-path _)
                         (canonicalize-path _)))
             ("line" . ,(source:line-for-user src))
             ("column" . ,(source:column src))))
         init))
   #f
   (regexp-quote (symbol->string sym))
   (apropos-fold-accessible ns)))

(define (get-lookup-information context)
  (let* ((state (assoc-ref context 'nrepl/state))
         (message (assoc-ref context 'nrepl/message))
         (reply (assoc-ref context 'reply))
         (ns (or (string->resolved-module (assoc-ref message "ns"))
                 (current-module)))
         (sym (and=> (assoc-ref message "sym") string->symbol)))
    (reply `(("status" . #("done"))
             ("info" . ,(lookup-symbol ns sym))))))

(define operations
  `(("lookup" . ,get-lookup-information)))

(define (wrap-lookup handler)
  (lambda (context)
    (let* ((message (assoc-ref context 'nrepl/message))
           (op (assoc-ref message "op"))
           (operation-function (assoc-ref operations op)))
      (if operation-function
          (operation-function context)
          (handler context)))))

(define lookup-extension
  `((name . "nrepl/lookup")
    (provides . (nrepl/lookup))
    (requires . (nrepl/session))
    (description . "Handles lookup related functionality.")
    (wrap . ,wrap-lookup)))
