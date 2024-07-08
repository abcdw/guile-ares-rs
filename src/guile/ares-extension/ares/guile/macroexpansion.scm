;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2024 Andrew Tropin
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

(define-module (ares-extension ares guile macroexpansion)
  #:use-module (ares guile)
  #:use-module (ares reflection modules)
  #:use-module ((system base compile) #:select (read-and-compile))
  #:export (ares.guile.macroexpansion))

(define (expand-macro module code)
  "Returns a pretty-printed representation of macro expansion."
  ((@@ (ares evaluation) object->pretty-string)
   (call-with-input-string
    (string-append "((@ (language tree-il) tree-il->scheme) (macroexpand '"
                   code
                   "))")
    (lambda (port)
      (read-and-compile port
                        #:to 'value
                        #:env module)))))

(define (sync-macroexpand-op context)
  "Syncronously expand provided macro."
  (let* ((message (assoc-ref context 'nrepl/message))
         (reply! (assoc-ref context 'reply!)))
    (with-exception-handler
     (lambda (ex)
       (reply! `(("error" . ,(format #f "~y" ex))
                 ("status" . #("error" "macroexpand-error" "done")))))
     (lambda ()
       (let ((module (or (string->resolved-module (assoc-ref message "module"))
                         (current-module)))
             (code (assoc-ref message "code")))
         (if (string? code)
             (reply! `(("status" . #("done"))
                       ("expansion" . ,(expand-macro module code))))
             (reply! `(("error" . "\
@code{code} argument is required and must be a string.")
                       ("status" . #("error" "macroexpand-error" "done")))))))
     #:unwind? #t)))

(define operations
  `(("ares.guile.macroexpansion/macroexpand" . ,sync-macroexpand-op)))

(define-with-meta (ares.guile.macroexpansion handler)
  "Allows to perform different macro expansions."
  `((provides . (ares.guile.macroexpansion))
    (requires . (ares/core ares/transport nrepl/session))
    (handles . ,operations))

  (lambda (context)
    (let* ((message (assoc-ref context 'nrepl/message))
           (op (assoc-ref message "op"))
           (operation-function (assoc-ref operations op)))
      (if operation-function
          (operation-function context)
          (handler context)))))
