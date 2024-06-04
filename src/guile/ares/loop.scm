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

(define (make-initial-context initial-extensions)
  (let ((state (make-atomic-box '()))
        (handler (make-atomic-box (make-handler initial-extensions))))
    ;; Threads Manager thread is created outside of fibers, so all the
    ;; threads created using threads-manager are not affected by
    ;; https://github.com/wingo/fibers/issues/105
    (define threads-manager (make-reusable-thread))
    (define (spawn-reusable-thread ch)
      (reusable-thread-discard-and-run
       threads-manager
       (lambda ()
         (make-reusable-thread ch)))
      (assoc-ref
       (reusable-thread-get-value threads-manager)
       'value))

    `((ares/spawn-reusable-thread . ,spawn-reusable-thread)
      (ares/state . ,state)
      (ares/handler . ,handler))))
;; Move to extension state?

(define (add-ports context input-port output-port)
  (append
   `((ares/input-port . ,input-port)
     (ares/output-port . ,output-port))
   context))

(define (loop context)
  "This loop will be executed in fibers environment,
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

  (let ((handler (car (atomic-box-ref (assoc-ref context 'ares/handler))))
        (input-port (assoc-ref context 'ares/input-port)))
    ;; Throws an error, when port get closed
    (false-if-exception
     (perform-operation (wait-until-port-readable-operation input-port)))

    (when (and
           (not (port-closed? input-port))
           (not (eof-object? (peek-char input-port))))
      (handler context)
      (loop context))))