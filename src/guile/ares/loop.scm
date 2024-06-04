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

(define-module (ares loop)
  #:use-module (fibers io-wakeup)
  #:use-module (fibers operations)
  #:use-module (ares extensions)
  #:use-module (ares reusable-thread)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 exceptions)
  #:export (make-initial-context add-ports loop))

(define module-documentation
  "\
This module contains utilities related to ares loop bootstrap: for
generating an initial context and starting the loop itself.")

(define (make-initial-context initial-extensions)
  "Generates and initial context, which contains @code{ares/state} and
@code{ares/handler}."
  (let ((state (make-atomic-box '()))
        (handler (make-atomic-box (make-handler initial-extensions))))
    ;; Pure Dynamic State is captured outside of fibers, so all the
    ;; threads created using spawn-reusable-thread are not affected by
    ;; https://github.com/wingo/fibers/issues/105
    (define pure-dynamic-state (current-dynamic-state))
    (define (spawn-reusable-thread ch)
      (with-dynamic-state pure-dynamic-state
        (lambda ()
          (make-reusable-thread ch))))

    `((ares/spawn-reusable-thread . ,spawn-reusable-thread)
      (ares/state . ,state)
      (ares/handler . ,handler))))

(define (add-ports context input-port output-port)
  "Return new context with @code{ares/input-port} and
@code{ares/output-port} added to the @code{context}."
  (append
   `((ares/input-port . ,input-port)
     (ares/output-port . ,output-port))
   context))

(define (loop context)
  "The loop waits for a @code{ares/input-port} to become available for
read and executes @code{ares/handler} on it.

The loop must be executed in fibers environment.

@code{ares/input-port}, @code{ares/output-port}, and
@code{ares/handler}, @code{ares/state} must be provided in the
@code{context}."
  (for-each
   (lambda (key)
     (unless (assoc key context)
       (raise-exception
        (make-exception
         (make-assertion-failure)
         (make-exception-with-message
          (format #f "\
Ares loop requires @code{~s} to be present in the @code{context}.
The actual value of the context is: @code{~s}"
                  key context))))))
   '(ares/input-port ares/output-port ares/handler ares/state))

  (let loop ((context context))
      (let ((handler (car (atomic-box-ref (assoc-ref context 'ares/handler))))
            (input-port (assoc-ref context 'ares/input-port)))
        ;; Throws an error, when port get closed
        (false-if-exception
         (perform-operation (wait-until-port-readable-operation input-port)))

        (when (and
               (not (port-closed? input-port))
               (not (eof-object? (peek-char input-port))))
          (handler context)
          (loop context)))))
