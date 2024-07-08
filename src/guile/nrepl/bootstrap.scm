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

(define-module (nrepl bootstrap)
  #:use-module (fibers io-wakeup)
  #:use-module (fibers operations)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 exceptions)
  #:use-module ((ares loop) :prefix loop:)
  #:use-module (ares extensions)
  #:use-module (ares ares-extensions guile-utils)
  #:use-module (nrepl ares-extensions completion)
  #:use-module (nrepl ares-extensions evaluation)
  #:use-module (nrepl ares-extensions lookup)
  #:use-module (nrepl ares-extensions session)
  #:use-module (ares-extension ares bencode)
  #:use-module (ares-extension ares core)
  #:use-module (ares-extension ares extension)

  #:export (bootstrap-nrepl bootstrap-extensions))

;;;
;;; Entry point for nrepl, setup basic state and fundamental extensions
;;;

(define bootstrap-extensions
  (list
   ;; TODO: [Andrew Tropin, 2023-09-25] Add extension extension
   ares.core
   ares.bencode
   ares.extension
   nrepl/session
   nrepl/completion
   nrepl/evaluation
   nrepl/lookup
   ares/guile-utils))

(define* (bootstrap-nrepl
          input-port output-port
          #:key
          (initial-extensions bootstrap-extensions))

  (let ((context (loop:add-ports
                  (loop:make-initial-context initial-extensions)
                  input-port output-port)))
    (loop:loop context)))

;; TODO: [Andrew Tropin, 2023-09-21] Initialize random number generator for
;; uuid

;; (set! *random-state* (random-state-from-platform))
