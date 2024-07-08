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

(define-module (ares nrepl bootstrap-test)
  #:use-module (ares ports)
  #:use-module (ares nrepl bootstrap)
  #:use-module (bencode)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (test-utils)
  #:use-module (fibers)
  #:use-module (fibers operations)
  #:use-module (fibers io-wakeup)
  #:use-module (ice-9 match)
  #:use-module (ares-extension ares core)
  #:use-module (ares-extension ares bencode)
  #:use-module (ares-extension nrepl evaluation)
  #:use-module (ares-extension nrepl session))

(define (repl-with-io-port start-repl function)
  (call-with-pipes
   (unbuffer-pipes! (make-pipes 2))
   (match-lambda
     (((input-input-port . input-output-port)
       (output-input-port . output-output-port))
      (run-fibers
       (lambda ()
         (spawn-fiber
          (lambda () (start-repl input-input-port output-output-port)))
         (function input-output-port output-input-port)))))))

(define (read-when-ready port)
  (perform-operation
   (wait-until-port-readable-operation port))
  (bencode->scm port))

(define (session-repl input-port output-port)
  (bootstrap-nrepl input-port output-port
                   #:initial-extensions
                   (list
                    ares.core
                    ares.bencode
                    nrepl.session)))

(define (compare-messages list1 list2)
 (lset= equal? list1 list2))

(define-test session-extension-test
  (test-group "session-extension"
    (repl-with-io-port
     session-repl
     (lambda (input output)
       (scm->bencode `(("op" . "clone")) input)

       (define session-id
         (assoc-ref
          (read-when-ready output) "new-session"))
       (test-assert "Received session-id" session-id)

       (scm->bencode `(("id". "2")
                       ("op" . "eval")
                       ("code" . "(+ 1 2)"))
                     input)
       (test-equal "Received unknow-op"
         `(("session" . "none")
           ("id" . "2")
           ("status" . #("error" "unknown-op" "done")))
         (read-when-ready output))

       (scm->bencode `(("id". "3")
                       ("op" . "close")
                       ("session" . ,session-id))
                     input)
       (test-equal "Received session-closed"
         `(("session" . ,session-id)
           ("id" . "3")
           ("status" . #("done" "session-closed")))
         (read-when-ready output))))))

(define (base-repl input-port output-port)
  (bootstrap-nrepl input-port output-port))

(define-test evaluation-extension-test
  (test-group "evaluation-extension"
    (repl-with-io-port
     base-repl
     (lambda (input output)
       (scm->bencode `(("op" . "clone")) input)

       (define session-id
         (assoc-ref
          (read-when-ready output) "new-session"))
       (test-assert "Received session-id" session-id)

       (scm->bencode `(("id". "2")
                       ("op" . "eval")
                       ("code" . "(+ 1 2)"))
                     input)
       (test-equal "Received error"
         `(("session" . "none")
           ("id" . "2")
           ("status" . #("error" "no-session-id-provided" "done")))
         (read-when-ready output))

       (scm->bencode `(("id". "3")
                       ("op" . "eval")
                       ("code" . "(+ 1 2 3)")
                       ("session" . ,session-id))
                     input)
       (test-equal "Received evaluation value"
         `(("session" . ,session-id)
           ("id" . "3")
           ("value" . "6")
           ("status" . #("done")))
         (read-when-ready output))))))
