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

(define-module (ares-extension ares extension)
  #:use-module (ares guile)
  #:use-module (ares extensions)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 match)
  #:use-module (ice-9 eval-string)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-197)
  #:export (ares.extension))

(define (add-extension context)
  "Tries to add an extension to the list of extensions and rebuild
@code{ares/handler}, if succeed the next and following iterations of
the loop will be using new handler."
  (let* ((message (assoc-ref context 'nrepl/message))
         (reply! (assoc-ref context 'reply!))
         (handler (assoc-ref context 'ares/handler))
         (state (atomic-box-ref (assoc-ref context 'ares/state)))
         (extensions-atom (assoc-ref state 'extensions))
         (extensions (atomic-box-ref extensions-atom))
         (new-extension (eval-string (assoc-ref message "extension")))
         (new-extensions (cons new-extension extensions))
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
         `(("status" . #("error" "cant-build-handler" "done")))))))

(define swap-extension
  (lambda (state message reply-function)
    'hi))

(define (describe context)
  "Provides a machine- and human-readable directory and documentation for
the operations supported by an nREPL endpoint."
  (let* ((state (atomic-box-ref (assoc-ref context 'ares/state)))
         (extensions (atomic-box-ref (assoc-ref state 'extensions)))
         (reply! (assoc-ref context 'reply!)))
    (define (get-extensions-directory extensions)
      (list->vector (map (lambda (e) (procedure-name e)) extensions)))
    (reply! `(("ops" . ,(list->vector
                         (map car (get-operations-directory extensions))))
              ("extensions" . ,(get-extensions-directory extensions))
              ("status" . #("done"))))))

(define operations
  `(("ares.extension/add-extension" . ,add-extension)
    ("ares.extension/describe" . ,describe)
    ("describe" . ,describe)))

(define-with-meta (ares.extension handler)
  "Handles extension related operations like extension-add and
 extensions-list."
  `((provides . (ares.extension))
    (requires . (ares.transport ares.state ares.core))
    (handles . ,operations))

  (lambda (context)
    (let* ((message (assoc-ref context 'nrepl/message))
           (operation (assoc-ref message "op"))
           (operation-function (assoc-ref operations operation)))
      (if operation-function
          (operation-function context)
          (handler context)))))
