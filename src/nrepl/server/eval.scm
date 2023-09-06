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
  #:use-module (ice-9 threads)
  #:export (output-stream-manager))


;;;
;;; I/O Handling
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
;; Probably strings, so we delay nrepl related logic further up the stack.
(define (output-stream-manager-thunk input-port output-channel
                                     process-finished-condition)
  "Watches INPUT-PORT and when something arrives sends it as a string to
the OUTPUT-CHANNEL.  Works until PROCESS-FINISHED-CONDITION is
signaled or INPUT-PORT is closed."
  (define (port-open? port) (not (port-closed? port)))
  (lambda ()
    (let loop ()
      (let ((op-value
             (perform-operation
              (choice-operation
               (wrap-operation
                (wait-until-port-readable-operation input-port)
                (const 'ready))
               (wrap-operation
                (wait-operation process-finished-condition)
                (const 'finished))))))

        ;; Try to read anyway, in case something came before process finished
        (when (and (port-open? input-port) (char-ready? input-port))
          (put-message output-channel (read-all-chars-as-string input-port)))

        ;; It doesn't make sense to keep watching port if it's already closed
        (if (and (equal? 'ready op-value) (port-open? input-port))
            (loop)
            'finished)))))

(define (make-pipes n)
  "Creates a list of N pipes."
  (map (lambda _ (pipe)) (iota n)))

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
      (lambda () (proc pipes))
    (lambda vals
      (close-pipes pipes)
      (apply values vals))))

(define (with-current-ports output-port error-port input-port thunk)
  (parameterize ((current-output-port output-port)
                 (current-error-port error-port)
                 (current-input-port input-port))
    (thunk)))


;;;
;;; Eval Thread
;;;

(define (make-evaluation-thread code finished-condition)
  "Evaluate code in a separate thread.  Signals FINISHED-CONDITION
 even if thread is cancelled."
  (call-with-new-thread
   (lambda ()
     (dynamic-wind
       (const #t)
       (lambda ()
         ;; file:~/work/gnu/guix/guix/repl.scm::`(exception (arguments ,key ,@(map value->sexp args))
         (with-exception-handler
             (lambda (exception)
               `((status . interrupted)
                 (exception-value . ,exception)
                 (stack . ,(make-stack #t))))
           (lambda ()
             `((status . done)
               (eval-value . ,(primitive-eval code))))
           #:unwind? #t))
       (lambda () (signal-condition! finished-condition))))))
