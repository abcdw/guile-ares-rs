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

(define-module (nrepl bootstrap)
  #:use-module (ares reusable-thread)
  #:use-module (fibers io-wakeup)
  #:use-module (fibers operations)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 exceptions)
  #:use-module (ares extensions)
  #:use-module (ares ares-extensions bencode)
  #:use-module (ares ares-extensions load-path)
  #:use-module (ares ares-extensions state)
  #:use-module (nrepl ares-extensions completion)
  #:use-module (nrepl ares-extensions evaluation)
  #:use-module (nrepl ares-extensions lookup)
  #:use-module (nrepl ares-extensions session)
  #:export (bootstrap-nrepl
            make-initial-context
            add-ports
            loop))

;;;
;;; Entry point for nrepl, setup basic state and fundamental extensions
;;;

(define bootstrap-extensions
  (list
   ;; TODO: [Andrew Tropin, 2023-09-25] Add extension extension
   state-extension
   bencode-extension
   session-extension
   completion-extension
   evaluation-extension
   lookup-extension
   load-path-extension))

;; Move to extension state?
(define (initial-context initial-extensions)
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
      (nrepl/state . ,state)
      (nrepl/handler . ,handler))))

(define (make-initial-context)
  (initial-context bootstrap-extensions))

(define (add-ports context input-port output-port)
  (append
   `((nrepl/input-port . ,input-port)
     (nrepl/output-port . ,output-port))
   context))

;; TODO: [Andrew Tropin, 2024-06-04] Rename all keys to ares/
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
Ares loop requires @code{~s} to be present in the @code{context}"
                  key))))))
   '(nrepl/input-port nrepl/output-port nrepl/handler nrepl/state))

  (let ((handler (car (atomic-box-ref (assoc-ref context 'nrepl/handler))))
        (input-port (assoc-ref context 'nrepl/input-port)))
    ;; Throws an error, when port get closed
    (false-if-exception
     (perform-operation (wait-until-port-readable-operation input-port)))

    (when (and
           (not (port-closed? input-port))
           (not (eof-object? (peek-char input-port))))
      (handler context)
      (loop context))))

(define* (bootstrap-nrepl
          input-port output-port
          #:key
          (initial-extensions bootstrap-extensions))

  (let ((context (add-ports (initial-context initial-extensions)
                            input-port output-port)))
    (loop context)))

;; TODO: [Andrew Tropin, 2023-09-21] Initialize random number generator for
;; uuid

;; (set! *random-state* (random-state-from-platform))
