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

(define-module (nrepl server eval)
  #:use-module (fibers operations)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (fibers io-wakeup)
  #:export (output-stream-manager))


;;;
;;; Output Stream Handling
;;;

(define (read-all-chars-as-string port)
  "Reads all available characters from the PORT and returns a string produced
 out of them."
  (with-output-to-string
    (lambda ()
      (let loop ()
        (when (char-ready? port)
          (write-char (read-char port))
          (loop))))))

;; Do channel accepts nrepl messages or just strings?
;; Probably strings, so we delay nrepl related logic further to top level.


(define (output-stream-manager port channel finished-condition)
  (lambda ()
    (let loop ()
      (let ((op-value
             (perform-operation
              (choice-operation
               (wrap-operation
                (wait-until-port-readable-operation port)
                (const 'ready))
               (wrap-operation
                (wait-operation finished-condition)
                (const 'finished))))))
        ;; Do we need to handle #<eof> somehow? even it appears

        ;; Try to read anyway, in case something
        (when (char-ready? port)
          (put-message channel (read-all-chars-as-string port)))
        (if (equal? 'ready op-value)
            (loop)
            (close-port port))))))
