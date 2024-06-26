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
  #:use-module (ares extensions)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 match)
  #:use-module (ice-9 eval-string)
  #:use-module (srfi srfi-197)
  #:export (extension-extension))

(define add-extension
  (lambda (handler state message reply!)
    (let* ((extensions-atom (assoc-ref (atomic-box-ref state) 'extensions))
           (old-extensions (atomic-box-ref extensions-atom))
           (new-extension (eval-string (assoc-ref message "extension")))
           (new-extensions (cons new-extension old-extensions))
           (new-handler (false-if-exception (make-handler new-extensions))))
      ;; (format #t "~y" new-extensions)
      (if new-handler
          (begin
            ;; TODO: [Andrew Tropin, 2024-06-26] Add check that
            ;; extension already exists in the list
            (atomic-box-set! handler new-handler)
            (atomic-box-set! extensions-atom new-extensions)
            (reply!
             `(("status" . #("done")))))
          (reply!
           ;; TODO: [Andrew Tropin, 2024-06-26] Send exception/problem back
           `(("status" . #("error" "cant-build-handler" "done"))))))))

(define swap-extension
  (lambda (state message reply-function)
    'hi))

(define (wrap-extension handler)
  (lambda (context)
    (let ((handler-atom (assoc-ref context 'ares/handler))
          (state (assoc-ref context 'ares/state))
          (reply! (assoc-ref context 'reply!))
          (message (assoc-ref context 'nrepl/message)))
      (match (assoc-ref message "op")
        ("ares/add-extension"
         (add-extension handler-atom state message reply!))
        (_ (handler context))))))

(define operations
  '(("ares/add-extension" . add-extension)
    ;; ("extension/swap" . identity)
    ;; ("extension/remove" . identity)
    ;; ("extension/list" . identity)
    ))

(define extension-extension
  `((name . "ares/extension")
    (provides . (ares/extension))
    (requires . (ares/transport ares/state ares/core))
    (description . "Handles extension related operations like extension-add and
 extensions-list.")
    (handles . ,operations)
    (wrap . ,wrap-extension)))
