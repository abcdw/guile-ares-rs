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

(define-module (nrepl server eval-test)
  #:use-module (nrepl server eval)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers timers)
  #:use-module (fibers conditions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define-syntax define-test
  (syntax-rules ()
    ((_ test-name e ...)
     (begin
       (define-public (test-name) e ...)
       (set-procedure-property! test-name 'srfi-64-test? #t)))))

;; TODO: Make a printer test, which genenrates eval responses with
;; stdout stderr content.

(define (alist-include? template data)
  "Check if all of the TEMPLATE's key-values pairs are presesnt in DATA."
  (lset<= equal? template data))

(define* (quickly operation
                  #:key (timeout 1))
  (perform-operation
   (choice-operation
    (sleep-operation timeout)
    operation)))

(define-test test-output-stream-manager
  (let* ((msg-ch (make-channel))
         (stdout (current-output-port))
         (finished-cnd (make-condition))
         (msgs-recieved (make-condition))
         (pout (pipe))
         (_ (setvbuf (cdr pout) 'none)))
    (run-fibers
     (lambda ()
       (spawn-fiber
        (output-stream-manager (car pout) msg-ch finished-cnd))
       (spawn-fiber
        (lambda ()
          (parameterize ((current-output-port (cdr pout)))
            (display "hi"))
          (signal-condition! finished-cnd)))
       (test-group
           "Testing Output Stream Manager"
           (test-equal "Received message hi"
             "hi" (get-message msg-ch))))
     #:drain? #t)))

;; (test-output-stream-manager)
