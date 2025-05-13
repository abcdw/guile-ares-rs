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

(define-module (ares-extension ares guile evaluation)
  #:use-module (ares-extension nrepl session)
  #:use-module (ares guile)
  #:use-module (ares evaluation supervisor)
  #:export (ares.guile.evaluation))

(define (process-message context)
  "Process eval related operation."
  (let* ((state (assoc-ref context 'ares/state))
         (message (assoc-ref context 'nrepl/message))
         (pure-dynamic-state (assoc-ref context 'ares/pure-dynamic-state))
         (reply! (assoc-ref context 'reply!))
         (session-id (assoc-ref message "session"))
         (session-atom (get-session state session-id)))
    (if session-id
        (evaluation-supervisor-process-nrepl-message
         (get-or-create-evaluation-supervisor!
          session-atom pure-dynamic-state)
         message reply!)
        (reply! `(("status" . #("error" "no-session-id-provided" "done")))))))

(define operations
  `(("ares.guile.evaluation/eval" . ,process-message)
    ("ares.guile.evaluation/stdin" . ,process-message)
    ("ares.guile.evaluation/interrupt" . ,process-message)))

(define-with-meta (ares.guile.evaluation handler)
  "Handles extended evaluation capabilites."
  `((provides . (ares.guile.evaluation))
    (requires . (nrepl.session))
    (handles . ,operations))

  (lambda (context)
    (let* ((message (assoc-ref context 'nrepl/message))
           (operation (assoc-ref message "op"))
           (operation-function
            (assoc-ref operations operation)))
      (if operation-function
          (operation-function context)
          (handler context)))))
