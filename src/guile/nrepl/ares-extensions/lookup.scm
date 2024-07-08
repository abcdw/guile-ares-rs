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

(define-module (nrepl ares-extensions lookup)
  #:use-module (ares file)
  #:use-module (ares guile)
  #:use-module (ares reflection metadata)
  #:use-module (ares reflection modules)
  #:use-module ((ice-9 regex) #:select (regexp-quote))
  #:use-module ((ice-9 session) #:select (apropos-fold
                                          apropos-fold-accessible))
  #:use-module (srfi srfi-197)
  #:use-module (srfi srfi-2)
  #:use-module (system vm program)
  #:export (nrepl/lookup))

(define (lookup-symbol ns sym)
  (define (module-location module)
    `(0 ,(module-filename module) 0 . 0))

  (apropos-fold
   (lambda (module name var init)
     (let* ((src (or (get-source var)
                     (and=> module module-location)))
            (file (chain-and
                   (source:file src)
                   (search-in-load-path _)))
            (line (and=> src source:line-for-user))
            (column (and=> src source:column))
            (arglists (get-arglists var))
            (docstring (get-docstring var)))
       (chain-when
        `(("ns" . ,(object->string (module-name module))))
        (file (acons "file" file _))
        (line (acons "line" line _))
        (column (acons "column" column _))
        (arglists (acons "arglists" arglists _))
        (docstring (acons "docstring" docstring _)))))
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
  (let* ((state (assoc-ref context 'ares/state))
         (message (assoc-ref context 'nrepl/message))
         (reply! (assoc-ref context 'reply!)))
    (with-exception-handler
        (lambda (ex)
          (reply! `(("status" . #("error" "lookup-error" "done")))))
      (lambda ()
        (let ((ns (or (string->resolved-module (assoc-ref message "ns"))
                      (current-module)))
              (sym (and=> (assoc-ref message "sym") string->symbol)))
          (reply! `(("status" . #("done"))
                    ("info" . ,(lookup-symbol ns sym))))))
      #:unwind? #t)))

(define operations
  `(("lookup" . ,get-lookup-information)))

(define-with-meta (nrepl/lookup handler)
  "Handles lookup related functionality."
  `((provides . (nrepl/lookup))
    (requires . (nrepl.session))
    (handles . ,operations))

  (lambda (context)
    (let* ((message (assoc-ref context 'nrepl/message))
           (op (assoc-ref message "op"))
           (operation-function (assoc-ref operations op)))
      (if operation-function
          (operation-function context)
          (handler context)))))
