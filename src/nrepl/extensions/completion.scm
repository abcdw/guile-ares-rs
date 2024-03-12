;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
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

(define-module (nrepl extensions completion)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 session)
  #:use-module (srfi srfi-197)
  #:export (completion-extension))


(define (->nrepl-completion-entry x)
  ;; (module-ref (current-module) 'string-append)
  `(("candidate" . ,x)
    ("type" . "function")))

(define (simple-completions prefix module)
  (save-module-excursion
   (lambda ()
     (set-current-module module)
     (if (string? prefix)
         (let ((prefix (string-append "^" (regexp-quote prefix))))
           (chain
            (apropos-internal prefix)
            (map symbol->string _)
            (sort! _ string<?)
            (map ->nrepl-completion-entry _)
            (list->vector _)))
         #()))))

(define (get-completions context)
  (let* ((state (assoc-ref context 'nrepl/state))
         (message (assoc-ref context 'nrepl/message))
         (ns (assoc-ref message "ns"))
         (module
          (or
           (if ns
               (resolve-module (with-input-from-string ns read) #:ensure #f)
               #f)
           (current-module)))
         (reply (assoc-ref context 'reply))
         (prefix (or (assoc-ref message "prefix") ""))
         (completions (simple-completions prefix module)))
    (reply `(("completions" . ,completions)
             ("status" . #("done"))))))

(define (find-file path)
  (let loop ((dirs %load-path))
    (if (null? dirs) #f
        (let ((candidate (string-append (car dirs) "/" path)))
          (if (file-exists? candidate)
              (canonicalize-path candidate)
              (loop (cdr dirs)))))))

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
