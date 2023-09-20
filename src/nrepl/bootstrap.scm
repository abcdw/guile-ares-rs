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

(define-module (nrepl bootstrap)
  #:use-module (ice-9 atomic)
  #:use-module (nrepl extensions)
  #:use-module (nrepl extensions state)
  #:use-module (nrepl extensions bencode)
  #:use-module (nrepl extensions session)
  #:export (bootstrap-nrepl))

;;;
;;; Entry point for nrepl, setup basic state and fundamental extensions
;;;

(define default-extensions
  `(extension
    bencode-transport
    session
    eval))

(define* (bootstrap-nrepl
          input-port output-port
          #:key
          (initial-extensions
           (list
            state-extension
            bencode-extension
            session-extension)))

  (let ((state (make-atomic-box '()))
          (handler (make-atomic-box (make-handler initial-extensions))))
      (let loop ()
        ((car (atomic-box-ref handler))
         `((nrepl/input-port . ,input-port)
           (nrepl/output-port . ,output-port)
           (nrepl/state . ,state)
           (nrepl/handler . ,handler)))

        (when (not (eof-object? (peek-char input-port)))
          (loop)))))
