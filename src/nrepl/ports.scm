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

(define-module (nrepl ports)
  #:export (read-all-chars-as-string
            make-pipes
            unbuffer-pipes!
            close-pipes
            call-with-pipes
            with-current-ports))

(define (read-all-chars-as-string port)
  "Reads all available characters from the PORT and returns a string produced
 out of them."
  (with-output-to-string
    (lambda ()
      (let loop ()
        (when (char-ready? port)
          (write-char (read-char port))
          (loop))))))

(define (make-pipes n)
  "Creates a list of N pipes."
  (map (lambda _ (pipe)) (iota n)))

(define (unbuffer-pipes! pipes)
  (for-each (lambda (p) (setvbuf (cdr p) 'none)) pipes)
  pipes)

(define (close-pipes pipes)
  "Takes a list of pipes and close all the related ports."
  (define (close-pipe p)
    ;; the order is important, otherwise it can lead to
    ;; fport_write: Broken pipe
    (close-port (cdr p))
    (close-port (car p)))
  (map (lambda (p) (close-pipe p)) pipes))

(define (call-with-pipes pipes proc)
  (call-with-values
      ;; MAYBE: [Andrew Tropin, 2023-09-06] Handle non-local exit?
      (lambda () (proc pipes))
    (lambda vals
      (close-pipes pipes)
      (apply values vals))))

(define (with-current-ports output-port error-port input-port thunk)
  (parameterize ((current-output-port output-port)
                 (current-error-port error-port)
                 (current-input-port input-port))
    (thunk)))
