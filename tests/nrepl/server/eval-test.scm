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
                  #:key
                  (timeout 1)
                  (default-value 'nothing))
  (perform-operation
   (choice-operation
    (wrap-operation
     (sleep-operation timeout)
     (const default-value))
    operation)))

(define-test test-output-stream-manager
  (test-group "Testing Output Stream Manager"
    (run-fibers
     (lambda ()
       (let* ((message-channel (make-channel))
              (finished-condition (make-condition))
              (stdout-pipe (pipe))
              (stdout-input-port (car stdout-pipe))
              (stdout-output-port (cdr stdout-pipe))
              (_ (setvbuf stdout-output-port 'none)))

         (spawn-fiber
          (output-stream-manager-thunk stdout-input-port
                                       identity
                                       message-channel finished-condition))

         (spawn-fiber
          (lambda ()
            (parameterize ((current-output-port stdout-output-port))
              (display "hi"))
            (signal-condition! finished-condition)))

         (test-equal "Received message hi"
           "hi" (get-message message-channel))

         (close-port stdout-input-port)
         (close-port stdout-output-port)

         (test-assert "Pipe Closed (input)"
           (port-closed? stdout-input-port))
         (test-assert "Pipe Closed (output)"
           (port-closed? stdout-output-port)))))
    #:drain? #t))

(define-test test-evaluation-manager
  (test-group "Testing Eval Manager"
    (test-group "Simple Evaluation with output streams capture"
      (define sample-code
        `(begin
           (display "hi-err" (current-error-port))
           (display "hi-out")
           'code-value))
      (run-fibers
       (lambda ()
         (let* ((downstream-channel (make-channel)))

           (spawn-fiber
            (evaluation-manager-thunk sample-code
                                      downstream-channel))
           ;; TODO: [Andrew Tropin, 2023-09-07] Read messages to queue
           ;; and perform checks on it.
           (test-equal "Received message hi-err"
             `(("err" . "hi-err")) (quickly (get-operation downstream-channel)))
           (test-equal "Received message hi-out"
             `(("out" . "hi-out")) (quickly (get-operation downstream-channel)))

           (test-equal "Received evaluation result"
             `(("status" . #("done"))
               ("value" . code-value))
             (quickly (get-operation downstream-channel)))))
       #:drain? #t))

    (test-group "Evaluation Interruption"
      (define sample-code-2
        `(begin
           (display "before sleep")
           (sleep 10)
           (display "after sleep")
           'code-value))
      (run-fibers
       (lambda ()
         (let* ((downstream-channel (make-channel))
                (interrupt-condition (make-condition)))

           (spawn-fiber
            (evaluation-manager-thunk sample-code-2
                                      downstream-channel
                                      #:interrupt-condition interrupt-condition))

           (test-equal "Received message hi-out"
             `(("out" . "before sleep"))
             (quickly (get-operation downstream-channel)))

           (signal-condition! interrupt-condition)

           (test-equal "Received evaluation interrupt"
             `(("status" . #("done" "interrupted")))
             (quickly (get-operation downstream-channel)))))
       #:drain? #t))))
