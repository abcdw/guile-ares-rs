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

(define-module (ares-extension ares guile meta-commands)
  #:use-module (ares guile)
  #:use-module (ares meta-commands)
  #:use-module (ares-extension nrepl evaluation)
  #:export (ares.guile.meta-commands))

;; FIXME: this could probably be improved to send number of required
;; arguments.
(define (list-meta-commands context _)
  "Lists available meta commands."
  (let ((reply! (assoc-ref context 'reply!))
        (names (hash-fold
                (lambda (key value acc)
                  (cons key acc))
                '()
                *command-infos*)))
    (reply! `(("names" . ,(list->vector names))
              ("status" . #("done"))))))

(define (call-meta-command-operation context handler)
  (let* ((reply! (assoc-ref context 'reply!))
         (message (assoc-ref context 'nrepl/message))
         (name (assoc-ref message "command"))
         (arguments (string-append (assoc-ref message "arguments") "\n"))
         (code `((@ (ares meta-commands) call-meta-command) ,name ,arguments))
         ;; FIXME: It would be better to not call nrepl.evaluation
         ;; directly. Its better to transform the message and pass it
         ;; down the extension stack.
         (evaluator (nrepl.evaluation handler)))
    (evaluator
     (acons
      'nrepl/message
      `(("op" . "eval")
        ("code" . ,(format #f "~s" code))
        ,@message)
      context))))

(define operations
  `(("ares.guile.meta-commands/list" . ,list-meta-commands)
    ("ares.guile.meta-commands/call" . ,call-meta-command-operation)))

(define-with-meta (ares.guile.meta-commands handler)
  "Handles evaluation related functionality."
  `((provides . (ares.guile.meta-commands))
    (requires . ())
    (handles . ,operations))

  (lambda (context)
    (let* ((message (assoc-ref context 'nrepl/message))
           (operation-function
            (assoc-ref operations (assoc-ref message "op"))))
      (if operation-function
          (operation-function context handler)
          (handler context)))))
