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

(define-module (nrepl atomic)
  #:use-module (ice-9 atomic)
  #:export (atomic-box-update!))

(define (atomic-box-update! box proc)
  "Atomically update the value of BOX to (PROC BOX-VALUE) and return the
new value.  PROC may be called multiple times, and thus PROC should be
free of side effects."
  (let loop ((old-value (atomic-box-ref box)))
    (let* ((new-value (proc old-value))
           (cas-value (atomic-box-compare-and-swap! box old-value new-value)))
      (if (eq? old-value cas-value)
          new-value
          (loop cas-value)))))
