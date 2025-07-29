;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2024 Andrew Tropin
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

(define-module (ares-extension ares logging)
  #:use-module (ares guile)
  #:use-module (ares atomic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-197)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 format)
  #:export (ares.logging))

(define (ppk prefix x)
  (format #t "~a~y\n" prefix x)
  x)

(define (shrink-value val)
  (let ((str (if (string? val) val (object->string val)))
        (width 60))
    (if (> (string-length str) width)
        (chain str (string-take _ width) (string-append _ "..."))
        val)))

(define (shrink-alist lst)
  (fold
   (lambda (kv res)
     (acons (car kv) (shrink-value (cdr kv)) res))
   '()
   lst))

;; TODO: [Andrew Tropin, 2024-05-24] Add operations for controlling
;; logging: enable/disable, supress some operations or fields in messages.

(define (set-verbosity-level! state-atom verbosity)
  (atomic-box-update!
   state-atom
   (lambda (state)
     (chain state
       (alist-delete 'ares.logging/verbosity _)
       (alist-cons 'ares.logging/verbosity verbosity _)))))

;; quite/silent normal/moderate verbose

;; (set-verbosity-level! context-accessor 'quite)

(define (get-verbosity state)
  (assoc-ref (atomic-box-ref state) 'ares.logging/verbosity))

(define (wrap-reply-with-logging state original-reply!)
  (lambda (reply-message)
    "Reply! wrapper from @code{ares.logging}."
    (when (equal? 'normal (get-verbosity state))
      (ppk "<= " (shrink-alist reply-message)))
    (original-reply! reply-message)))

(define (log-incomming-message state message)
  (when (equal? 'normal (get-verbosity state))
    (ppk "=> " (shrink-alist message))))

(define-with-meta (ares.logging handler)
  "Prints @code{nrepl/message} and wraps @code{reply!} function to log
 outgoing nREPL messages."
  `((provides . (ares.logging))
    (requires . (ares.core ares.transport)))

  (lambda (context)
    (let* ((message (assoc-ref context 'nrepl/message))
           (state (assoc-ref context 'ares/state))
           (original-reply! (assoc-ref context 'reply!))
           (wrapped-reply! (wrap-reply-with-logging state original-reply!)))
      (log-incomming-message state message)
      (handler (acons 'reply! wrapped-reply! context)))))
