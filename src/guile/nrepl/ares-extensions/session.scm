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

(define-module (nrepl ares-extensions session)
  #:use-module (fibers conditions)
  #:use-module (ice-9 atomic)
  #:use-module (ares atomic)
  #:use-module (ares alist)
  #:use-module (ares guile)
  #:use-module (uuid)
  #:use-module (srfi srfi-197)
  #:export (nrepl/session
            get-session))

(define (register-session! state-atom session-id new-session)
  (atomic-box-update!
   state-atom
   (lambda (state)
     (let* ((sessions (or (assoc-ref state 'sessions) '()))
            (new-sessions (alist-cons session-id new-session sessions)))
       (chain state
         (alist-delete 'sessions _)
         (alist-cons 'sessions new-sessions _))))))

(define (unregister-session! state-atom session-id)
  (atomic-box-update!
   state-atom
   (lambda (state)
     (let* ((sessions (or (assoc-ref state 'sessions) '()))
            (new-sessions (alist-delete session-id sessions)))
       (chain state
         (alist-delete 'sessions _)
         (alist-cons 'sessions new-sessions _))))))

(define (response-for message reply!)
  (let ((id (assoc-ref message "id"))
        (session (assoc-ref message "session")))
    (chain reply!
           (acons "id" (or id "unknown") _)
           (acons "session" (or session "none") _))))

(define (make-new-session)
  ;; Downstream extensions can rely on this condition to know, when
  ;; they have to shutdown.
  (let* ((shutdown-condition (make-condition))
         (shutdown (lambda () (signal-condition! shutdown-condition))))
    (make-atomic-box `((shutdown-condition . ,shutdown-condition)
                       (shutdown . ,shutdown)))))

(define (get-session state-atom session-id)
  (alist-get-in
   `(sessions ,session-id)
   (atomic-box-ref state-atom)))

(define (clone-session context)
  (let ((new-session-id (uuid))
        (new-session (make-new-session))
        (state (assoc-ref context 'ares/state))
        (reply! (assoc-ref context 'reply!)))
    (register-session! state new-session-id new-session)
    (reply!
     `(("status" . #("done"))
       ("new-session" . ,new-session-id)))))

(define (close-session context)
  (let* ((state (assoc-ref context 'ares/state))
         (message (assoc-ref context 'nrepl/message))
         (session-id (assoc-ref message "session"))
         (session (and=> (get-session state session-id) atomic-box-ref))
         (reply! (assoc-ref context 'reply!)))
    (if session
        (begin
         ((assoc-ref session 'shutdown))
         (unregister-session! state session-id)
         (reply!
          `(("status" . #("done" "session-closed")))))
        (reply!
         `(("status" . #("error" "no-such-session" "done")))))))

(define operations
  `(("clone" . ,clone-session)
    ("close" . ,close-session)))

(define-with-meta (nrepl/session handler)
  "Handles session related operations like clone and
 ls-sessions."
  `((requires . (ares.state ares.transport))
    (provides . (nrepl/session))
    (handles . ,operations)
    (implements . ((session/reply!)))
    (upgrades . ((reply!))))

  (define (add-session-reply-function context)
    (let* ((message (assoc-ref context 'nrepl/message))
           (original-reply! (assoc-ref context 'reply!))
           (session-reply! (lambda (reply)
                             (original-reply! (response-for message reply)))))
      (chain context
             (acons 'session/reply! session-reply! _)
             (acons 'reply! session-reply! _))))

  (lambda (context)
    (let* ((state (assoc-ref context 'ares/state))
           (message (assoc-ref context 'nrepl/message))
           (new-context (add-session-reply-function context))
           (session-id (assoc-ref message "session"))
           (operation-function
            (assoc-ref operations (assoc-ref message "op"))))
      (if operation-function
          (operation-function new-context)
          ;; Short-circuit if there is no such session
          (if (and session-id (not (get-session state session-id)))
              ((assoc-ref new-context 'reply!)
               `(("status" . #("error" "unknown-session" "done"))))
              (handler new-context))))))
