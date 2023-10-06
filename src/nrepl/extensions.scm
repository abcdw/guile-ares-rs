;;; guile-nrepl --- Asynchronous Reliable Extensible Scheme RPC Server
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

(define-module (nrepl extensions)
  #:use-module (ice-9 atomic)
  #:use-module (srfi srfi-1)
  #:export (make-handler))

(define (unknown-op context)
  (let ((reply
         (or
          (assoc-ref context 'reply)
          (assoc-ref context 'transport/reply))))
    (reply `(("status" . #("error" "unknown-op" "done"))))))

(define (sort-extensions extentions)
  ;; TODO: [Andrew Tropin, 2023-09-20] Ensure transport is present
  ;; TODO: [Andrew Tropin, 2023-09-20] Implement dependency resolution
  (reverse extentions))

(define (make-handler extensions)
  (cons
   (fold (lambda (extension handler)
           ((assoc-ref extension 'wrap) handler))
         unknown-op
         (sort-extensions extensions))
   extensions))
