;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2025 Libre en Communs <contact@a-lec.org>
;;; Copyright © 2025 Noé Lopez <noelopez@free.fr>
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

(define-module (ares evaluation io-test)
  #:use-module (ares evaluation io)
  #:use-module (ares evaluation test-utils)
  #:use-module (srfi srfi-64)
  #:use-module ((ice-9 textual-ports) #:select (put-string))
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (test-utils))

(define-test test-output-ports
  (run-fibers
   (lambda ()
     (define channel (make-channel))
     (spawn-fiber
      (lambda ()
        (define port (open-channel-output-port channel))
        (put-string port "hello")
        (close port)))
     (test-equal "received output" '(output "hello") (get-message channel)))))

(define-test test-input-ports
  (run-fibers
   (lambda ()
     (define input-channel (make-channel))
     (define request-channel (make-channel))
     (define port (open-channel-input-port request-channel input-channel))

     (spawn-fiber
      (lambda ()
        (test-equal "received input request"
          '((action . need-input))
          (quickly (get-operation request-channel)))
        (put-message input-channel "\"hello\"")))

     (test-equal "received input" "hello" (read port)))))
