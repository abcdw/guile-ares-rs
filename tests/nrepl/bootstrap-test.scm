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
  #:use-module (nrepl bootstrap)
  #:use-module (bencode)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (test-utils)
  #:use-module (nrepl extensions state)
  #:use-module (nrepl extensions bencode)
  #:use-module (nrepl extensions session))

(define (check-bootstrap)
  (let ((input-port (open-input-string
                     (string-append
                      (scm->bencode-string
                       `(("id". 1)
                         ("op" . "clone")))
                      (scm->bencode-string
                       `(("id". 2)
                         ("op" . "eval")
                         ("code" . "(+ 1 2)"))))))
        (output-port (open-output-string)))

    (bootstrap-nrepl input-port output-port)

    (call-with-input-string (get-output-string output-port)
      (lambda (port)
        (let loop ()
          ((@ (ice-9 pretty-print) pretty-print)
           (bencode->scm port))
          (newline)
          (when (not (eof-object? (peek-char port)))
            (loop)))))))

(define* (run-repl
          input-messages
          #:key
          (extensions
           (list
            state-extension
            bencode-extension
            session-extension)))
  (let ((input-port (open-input-string
                     (fold (lambda (x result)
                             (string-append result (scm->bencode-string x)))
                           ""
                           input-messages)))
        (output-port (open-output-string)))

    (bootstrap-nrepl input-port output-port
                     #:initial-extensions extensions)

    (reverse
     (call-with-input-string (get-output-string output-port)
       (lambda (port)
         (let loop ((result '()))
           (if (not (eof-object? (peek-char port)))
               (loop (cons (bencode->scm port) result))
               result)))))))

(define (compare-messages list1 list2)
 (lset= equal? list1 list2))

(define-test session-extension-test
  (define input-messages
    `((("id". "1")
       ("op" . "clone"))
      (("id". "2")
       ("op" . "eval")
       ("code" . "(+ 1 2)"))))

  (define expected-output-messages
    `((("session" . "none")
       ("id" . "1")
       ("status" . #("done"))
       ("new-session"
        .
        "92ad654d-a72c-4e50-b55c-523fdebfe29c"))
      (("session" . "none")
       ("id" . "2")
       ("status" . #("error" "unknown-op" "done")))))

  (define output-messages
    (run-repl input-messages
              #:extensions
              (list
               state-extension
               bencode-extension
               session-extension)))

  (test-group
      "session-extension"
      (test-assert
          "clone and eval messages"
        (compare-messages expected-output-messages
                          output-messages))))
