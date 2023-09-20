;;; guile-nrepl --- Asyncronous Reliable Extensible Scheme Network REPL
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
;;;
;;; This file is part of guile-nrepl.
;;;
;;; guile-nrepl is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; guile-nrepl is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guile-nrepl.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nrepl extensions session)
  #:use-module (ice-9 atomic)
  #:use-module (srfi srfi-197)
  #:export (session-extension))

(define (response-for message reply)
  (let ((id (assoc-ref message "id"))
        (session (assoc-ref message "session")))
    (chain reply
           (acons "id" (or id "unknown") _)
           (acons "session" (or session "none") _))))

;; (define (clone-session sessions channel finished? input)
;;   (let ((new-session-id (uuid))
;;         (new-session #f))
;;     (register-session! sessions new-session-id new-session)
;;     (put-message
;;      channel
;;      (response-for
;;       input
;;       `(("status" . #("done"))
;;         ("new-session" . ,new-session-id))))
;;     (signal-condition! finished?)))


(define (add-session-reply context)
  (let* ((message (assoc-ref context 'nrepl/message))
         (transport-reply (assoc-ref context 'transport/reply))
         (session-reply (lambda (reply)
                          (transport-reply (response-for message reply)))))
    (chain context
      (acons 'session/reply session-reply _)
      (acons 'reply session-reply _))))

(define (wrap-session handler)
  (lambda (context)
    (let ((state (assoc-ref context 'nrepl/state))
          (message (assoc-ref context 'nrepl/message)))
      (case (assoc-ref message "op")
        ;; ("clone" clone-operation)
        ;; ("ls-sessions" ls-session-operation)
        (else (handler (add-session-reply context)))))))

(define session-extension
  `((name . "nrepl/session")
    (provides . (nrepl/session))
    (description . "Handles session related operations like clone and
 ls-sessions.")
    (handles . (("clone")
                ("session-clone")))
    (implements . ((session/reply)))
    (upgrades . ((reply)))
    (wrap . ,wrap-session)))
