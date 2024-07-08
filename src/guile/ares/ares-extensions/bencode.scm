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

(define-module (ares ares-extensions bencode)
  #:use-module (ares guile)
  #:use-module (bencode)
  #:use-module (srfi srfi-197)
  #:export (ares/bencode))

(define-with-meta (ares/bencode handler)
  "Add @code{transport/reply!} and @code{reply!} functions to context."
  `((requires . (ares.core ares.io))
    (provides . (ares/transport ares/bencode)))
  (lambda (context)
    (let* ((input-port (assoc-ref context 'ares/input-port))
           (output-port (assoc-ref context 'ares/output-port))
           (message (bencode->scm input-port))
           (transport-reply! (lambda (reply)
                              (scm->bencode reply output-port)
                              ;; Otherwise bencode message won't be
                              ;; flashed to the socket
                              (force-output output-port))))
      (handler
       (chain context
         ;; Why nrepl/message and not transport/message?  While the
         ;; message produced by transport underlying extension don't
         ;; care how it was added, it's already in scm.
         (acons 'nrepl/message message _)
         (acons 'transport/reply! transport-reply! _)
         (acons 'reply! transport-reply! _))))))
