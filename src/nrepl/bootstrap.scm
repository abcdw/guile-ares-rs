;;; guile-ares-rs --- Asynchronous Reliable Extensible Scheme RPC Server
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

(define-module (nrepl bootstrap)
  #:use-module (ice-9 atomic)
  #:use-module (fibers io-wakeup)
  #:use-module (fibers operations)
  #:use-module (nrepl extensions)
  #:use-module (nrepl extensions state)
  #:use-module (nrepl extensions bencode)
  #:use-module (nrepl extensions session)
  #:use-module (nrepl extensions evaluation)
  #:export (bootstrap-nrepl
            make-initial-context
            add-ports
            nrepl-loop))

;;;
;;; Entry point for nrepl, setup basic state and fundamental extensions
;;;

(define bootstrap-extensions
  (list
   ;; TODO: [Andrew Tropin, 2023-09-25] Add extension extension
   state-extension
   bencode-extension
   session-extension
   evaluation-extension))

;; Move to extension state?
(define (initial-context initial-extensions)
  (let ((state (make-atomic-box '()))
        (handler (make-atomic-box (make-handler initial-extensions))))
    `((nrepl/state . ,state)
      (nrepl/handler . ,handler))))

(define (make-initial-context)
  (initial-context bootstrap-extensions))

(define (add-ports context input-port output-port)
  (append
   `((nrepl/input-port . ,input-port)
     (nrepl/output-port . ,output-port))
   context))

(define (nrepl-loop context)
  (let ((handler (car (atomic-box-ref (assoc-ref context 'nrepl/handler))))
        (input-port (assoc-ref context 'nrepl/input-port)))
    (handler context)

    ;; Throws an error, when port get closed
    (false-if-exception
     (perform-operation (wait-until-port-readable-operation input-port)))

    (when (and
           (not (port-closed? input-port))
           (not (eof-object? (peek-char input-port))))
      (nrepl-loop context))))

(define* (bootstrap-nrepl
          input-port output-port
          #:key
          (initial-extensions bootstrap-extensions))

  (let ((context (add-ports (initial-context initial-extensions)
                            input-port output-port)))
    (nrepl-loop context)))

;; TODO: [Andrew Tropin, 2023-09-21] Initialize random number generator for
;; uuid

;; (set! *random-state* (random-state-from-platform))
