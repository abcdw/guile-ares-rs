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

(define-module (nrepl bootstrap-test)
  #:use-module (nrepl bootstrap))

(define (check-bootstrap)
  (let ((input-port (open-input-string
                     (string-append
                      ((@ (bencode) scm->bencode-string)
                       `(("id". 1)
                         ("op" . "clone")))
                      ((@ (bencode) scm->bencode-string)
                       `(("id". 2)
                         ("op" . "eval")
                         ("code" . "(+ 1 2)"))))))
        (output-port (open-output-string)))

    (bootstrap-nrepl input-port output-port)

    (call-with-input-string (get-output-string output-port)
      (lambda (port)
        (let loop ()
          ((@ (ice-9 pretty-print) pretty-print)
           ((@ (bencode) bencode->scm)
            port))
          (newline)
          (when (not (eof-object? (peek-char port)))
            (loop)))))))
