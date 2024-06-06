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

(define-module (ares ares-extensions state)
  #:use-module (fibers scheduler)
  #:use-module (ice-9 atomic)
  #:use-module (ares exceptions)
  #:export (state-extension))

;; State must be provided outside of extensions to be shared between
;; all loops.

(define (ensure-external-dependencies-provided handler)
  (define checked? #f)
  (define (perform-context-check context)
    (unless (atomic-box? (assoc-ref context 'ares/state))
      (raise-assert "\
Context must contain @code{ares/state} key with atomic-box inside."))

    (unless (and (assoc-ref context 'ares/input-port)
                 (assoc-ref context 'ares/output-port))
      (raise-assert "\
Context must contain @code{ares/input-port} and @code{ares/output-port} keys."))

    (unless (current-scheduler)
      (raise-assert "Ares loop must be executed in fibers context")))

  (lambda (context)
    (unless checked?
      (perform-context-check context)
      (set! checked? #t))
    (handler context)))

(define state-extension
  `((name . "ares/state")
    (provides . (ares/root ares/state fibers))
    (requires) ; the root extension has no requirements
    (description . "Signals that the state is provided, it's auxiliary
 extension, just to make other extension sure that state and fibers
 are available.")
    (wrap . ,ensure-external-dependencies-provided)))
