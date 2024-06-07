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

(define-module (ares ares-extensions extension)
  #:use-module (srfi srfi-197)
  #:export (extension-extension))

(define add-extension
  (lambda (state message reply-function)
    'hi))

(define swap-extension
  (lambda (state message reply-function)
    'hi))

(define wrap-extension
  (lambda (handler)
    (lambda (context)
      (let ((state (assoc-ref context 'ares/state))
            (reply! (assoc-ref context 'reply!))
            (message (assoc-ref context 'nrepl/message)))
        'hi
        ;; (case (assoc-ref message "op")
        ;;   ("add-extension"
        ;;    (add-extension state message reply-function))
        ;;   (else (handler message)))
        ))))

(define extension-extension
  `((name . "ares/extension")
    (provides . (ares/extension))
    (requires . (ares/transport ares/state ares/core))
    (description . "Handles extension related operations like extension-add and
 extensions-list.")
    (handles . (("extension/add" . add-extension)
                ("extension/swap" . identity)
                ("extension/remove" . identity)
                ("extension/list" . identity)))
    (wrap . ,wrap-extension)))
