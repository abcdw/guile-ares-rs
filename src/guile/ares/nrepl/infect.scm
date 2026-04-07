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

(define-module (ares nrepl infect)
  #:use-module (srfi srfi-1))

;;;
;;; Transform a usual network repl into nrepl
;;;

;; setup ares/state, nrepl/message and nrepl/reply-function
;; nrepl/transport
;; nrepl/extension

(define (get-file-content file)
  (call-with-input-file file
    (@ (ice-9 textual-ports) get-string-all)
    #:encoding "UTF-8"))

(define (prepare-dependencies files)
  (fold
   (lambda (element accumulator)
     (string-append
      accumulator
      (get-file-content
       (%search-load-path
        element))))
   ""
   files))

;; (get-file-content
;;  (%search-load-path "bencode"))
;; (get-file-content
;;  (%search-load-path "uuid"))

;; (display (prepare-dependencies (list "bencode" "uuid")))

(define mini-nrepl
  `((use-modules (ice-9 atomic))
    (define (break-the-repl))))

;; (define (my-repl-welcome repl)
;;   (display "Enter `,help' for help. My friend.\n"))
;; (set! (@ (system repl common) repl-welcome) my-repl-welcome)

;; (use-modules (system repl server))
