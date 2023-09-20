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

(define-module (nrepl extensions bencode)
  #:use-module (bencode)
  #:use-module (srfi srfi-197)
  #:export (bencode-extension))

(define (wrap-bencode handler)
  (lambda (context)
    (let* ((input-port (assoc-ref context 'nrepl/input-port))
           (output-port (assoc-ref context 'nrepl/output-port))
           (message (bencode->scm input-port))
           (transport-reply (lambda (reply) (scm->bencode reply output-port))))
      (handler
       (chain context
         ;; Why nrepl/message and not transport/message?  While the
         ;; message produced by transport underlying extension don't
         ;; care how it was added, it's already in scm.
         (acons 'nrepl/message message _)
         (acons 'transport/reply transport-reply _)
         (acons 'reply transport-reply _))))))

(define bencode-extension
  `((name . "nrepl/extension")
    (provides . (nrepl/transport nrepl/bencode))
    (description . "Add transport/reply and reply functions to context.")
    (wrap . ,wrap-bencode)))
