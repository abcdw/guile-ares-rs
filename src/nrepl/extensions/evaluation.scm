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

(define-module (nrepl extensions evaluation)
  #:use-module (nrepl extensions session)
  #:use-module (ice-9 atomic)
  #:use-module (nrepl atomic)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (nrepl server evaluation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-197)
  #:export (evaluation-extension))

(define (make-evaluation-supervisor session)
  (let ((control-channel (make-channel)))
    (spawn-fiber (evaluation-supervisor-thunk
                  control-channel
                  #:shutdown-condition (assoc-ref session 'shutdown-condition)))
    control-channel))

(define (create-evaluation-supervisor! session-atom)
  (let* ((tmp-evaluation-supervisor
          (make-evaluation-supervisor (atomic-box-ref session-atom)))
         (add-evaluation-supervisor
          (lambda (session)
            (let ((evaluation-supervisor
                   (assoc-ref session 'evaluation-supervisor)))
              (if evaluation-supervisor
                  session
                  (alist-cons
                   'evaluation-supervisor tmp-evaluation-supervisor session)))))
         (new-session
          (atomic-box-update! session-atom add-evaluation-supervisor))
         (new-evaluation-supervisor
          (assoc-ref new-session 'evaluation-supervisor)))
    (when (not (eq? new-evaluation-supervisor tmp-evaluation-supervisor))
      (evaluation-supervisor-shutdown tmp-evaluation-supervisor))
    new-evaluation-supervisor))

(define (get-or-create-evaluation-supervisor! session-atom)
  (let ((evaluation-supervisor (assoc-ref (atomic-box-ref session-atom)
                                          'evaluation-supervisor)))
    (if evaluation-supervisor
        evaluation-supervisor
        (create-evaluation-supervisor! session-atom))))

(define (process-message context)
  (let* ((state (assoc-ref context 'nrepl/state))
         (message (assoc-ref context 'nrepl/message))
         (reply (assoc-ref context 'reply))
         (session-id (assoc-ref message "session"))
         (session-atom (get-session state session-id)))
    (if session-id
        (evaluation-supervisor-process-nrepl-message
         (get-or-create-evaluation-supervisor! session-atom)
         message reply)
        (reply `(("status" . #("error" "no-session-id-provided" "done")))))))

(define operations
  `(("eval" . ,process-message)
    ("stdin" . ,process-message)
    ("interrupt" . ,process-message)))

(define (wrap-evaluation handler)
  (lambda (context)
    (let* ((message (assoc-ref context 'nrepl/message))
           (operation-function
            (assoc-ref operations (assoc-ref message "op"))))
      (if operation-function
          (begin
            (operation-function context)
            ;; (display "===============\n")
            ;; ((@ (ice-9 pretty-print) pretty-print) context)
            )
          (handler context)))))

(define evaluation-extension
  `((name . "nrepl/evaluation")
    (provides . (nrepl/evaluation))
    (requires . (nrepl/session fibers))
    (description . "Handles evaluation related functionality.")
    (wrap . ,wrap-evaluation)))
