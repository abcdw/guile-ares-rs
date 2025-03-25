;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023, 2024, 2025 Andrew Tropin <andrew@trop.in>
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

(define-module (ares-extension ares bencode)
  #:use-module (ares guile)
  #:use-module (bencode)
  #:use-module (srfi srfi-197)
  #:export (ares.bencode))

(define-with-meta (ares.bencode handler)
  "Add @code{transport/reply!} and @code{reply!} functions to context."
  `((requires . (ares.core ares.io))
    (provides . (ares.transport ares.bencode)))

  (define (add-reply-id message reply)
    (let ((id (assoc-ref message "id")))
      (if (assoc "id" reply)
          reply
          (acons "id" (or id "unknown") reply))))

  (lambda (context)
    (let* ((input-port (assoc-ref context 'ares/input-port))
           (output-port (assoc-ref context 'ares/output-port))
           (message (bencode->scm input-port))
           (transport-reply!
            (lambda (reply)
              (scm->bencode (add-reply-id message reply) output-port)
              ;; Otherwise bencode message won't be
              ;; flashed to the socket
              (force-output output-port)))
           (new-context
            (chain context
                   ;; Why nrepl/message and not transport/message?  While the
                   ;; message produced by transport underlying extension don't
                   ;; care how it was added, it's already in scm.
                   (acons 'nrepl/message message _)
                   (acons 'transport/reply! transport-reply! _)
                   (acons 'reply! transport-reply! _))))

      (with-exception-handler
       (lambda (ex)
         (transport-reply!
          `(("error" . ,((@@ (ares evaluation) object->pretty-string) ex))
            ("status" . #("error" "something-broken-after-ares-bencode"
                          "done")))))
       (lambda ()
         (handler new-context))
       #:unwind? #t))))
