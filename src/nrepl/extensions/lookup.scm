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
  #:use-module (ares reflection metadata)
  #:use-module (ares reflection modules)
  #:use-module ((ice-9 regex) #:select (regexp-quote))
  #:use-module ((ice-9 session) #:select (apropos-fold
                                          apropos-fold-accessible))
  #:use-module (srfi srfi-197)
  #:use-module (srfi srfi-2)
  #:use-module (system vm program)
  #:export (lookup-extension))

(define (lookup-symbol ns sym)
  (define (module-location module)
    `(0 ,(module-filename module) 0 . 0))

  (define (absolute-path path)
    (if (absolute-file-name? path)
        path
        (string-append (getcwd) "/" path)))

  (apropos-fold
   (lambda (module name var init)
     (let ((src (or (get-source var)
                    (and=> module module-location)))
           (docstring (get-docstring var))
           (arglists (get-arglists var)))
       `(("file" . ,(chain
                     (source:file src)
                     (%search-load-path _)
                     (absolute-path _)))
         ("line" . ,(source:line-for-user src))
         ("column" . ,(source:column src))
         ("ns" . ,(object->string (module-name module)))
         ("arglists" . ,arglists)
         ("docstring" . ,docstring))))
   #f
   ;; Instead of early return we just wrap regexp in ^$ to exactly
   ;; match the symbol we are interested in, to further speed up the
   ;; implementation it would be necessary to rewrite module traverse
   ;; in apropos-fold and use early return in it.
   (string-append "^" (regexp-quote (symbol->string sym)) "$")
   ;; apropos-fold-accessible leads to inconsistencies between lookup
   ;; and symbol resolution. That's why we need reverse here to prevent
   ;; incorrect symbol shadowing
   ((@@ (ice-9 session) make-fold-modules)
    (lambda () (list ns))
    (compose reverse module-uses)
    identity)))

(define (get-lookup-information context)
  (let* ((state (assoc-ref context 'nrepl/state))
         (message (assoc-ref context 'nrepl/message))
         (reply (assoc-ref context 'reply)))
    (with-exception-handler
     (lambda (ex)
       (reply `(("status" . #("done" "lookup-error")))))
     (lambda ()
       (let ((ns (or (string->resolved-module (assoc-ref message "ns"))
                     (current-module)))
             (sym (and=> (assoc-ref message "sym") string->symbol)))
         (reply `(("status" . #("done"))
                  ("info" . ,(lookup-symbol ns sym)))))))))

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
