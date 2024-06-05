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

(define-module (nrepl ares-extensions completion)
  #:use-module (ares reflection metadata)
  #:use-module (ares reflection modules)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 session)
  #:use-module (srfi srfi-197)
  #:export (completion-extension))

(define* (simple-completions prefix module #:optional options)
  (define (get-candidates)
    (let ((extra-meta
           (or (and=> (assoc-ref options "extra-metadata") vector->list)
               '())))
      (apropos-fold
       (lambda (module name var acc)
         (let ((cand
                (chain-when
                 `(("candidate" . ,(symbol->string name))
                   ("type" . ,(cond
                               ((macro? var) "macro")
                               ((procedure? var) "function")
                               (else "var")))
                   ("ns" . ,(object->string (module-name module))))
                 ((member "arglists" extra-meta)
                  (acons "arglists" (get-arglists var) _))
                 ((member "docs" extra-meta)
                  (acons "docs" (get-docstring var) _)))))
           (cons cand acc)))
       '()
       (string-append "^" (regexp-quote prefix))
       ((@@ (ice-9 session) make-fold-modules)
        (lambda () (list module))
	(compose reverse module-uses)
	identity))))

  (define (candidate<? a b)
    (string<? (assoc-ref a "candidate")
              (assoc-ref b "candidate")))

  (chain
   (get-candidates)
   (sort! _ candidate<?)
   (list->vector _)))

(define (get-completions context)
  (let* ((state (assoc-ref context 'ares/state))
         (reply! (assoc-ref context 'reply!))
         (message (assoc-ref context 'nrepl/message)))
    (with-exception-handler
        (lambda (ex)
          (reply! `(("status" . #("done" "completion-error")))))
      (lambda ()
        (let* ((module (or (string->resolved-module (assoc-ref message "ns"))
                           (current-module)))
               (prefix (or (assoc-ref message "prefix") ""))
               (options (assoc-ref message "options"))
               (completions (simple-completions prefix module options)))
          (reply! `(("completions" . ,completions)
                    ("status" . #("done"))))))
      #:unwind? #t)))

(define operations
  `(("completions" . ,get-completions)))

(define (wrap-completion handler)
  (lambda (context)
    (let* ((message (assoc-ref context 'nrepl/message))
           (operation-function (assoc-ref operations (assoc-ref message "op"))))
      (if operation-function
          (operation-function context)
          (handler context)))))

(define completion-extension
  `((name . "nrepl/completion")
    (provides . (nrepl/completion))
    (requires . (nrepl/session))
    (description . "Handles completion related functionality.")
    (wrap . ,wrap-completion)))
