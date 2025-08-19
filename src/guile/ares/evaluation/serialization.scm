;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023, 2024 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2024 Nikita Domnitskii <nikita@domnitskii.me>
;;; Copyright © 2025 Libre en Communs <contact@a-lec.org>
;;; Copyright © 2025 Noé Lopez <noelopez@free.fr>
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

;;; Commentary:

;;; Serialization of scheme values to bencode encodable s-expressions.

;;; Code:

(define-module (ares evaluation serialization)
  #:use-module (ares file)
  #:use-module (ares guile)
  #:use-module (ares guile exceptions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 and-let-star)
  #:use-module (srfi srfi-1)
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:export (exception->nrepl-messages
            evaluation-result->nrepl-messages
            multiple-values->nrepl-messages
            interrupt-result->nrepl-messages
            frame->nrepl-value
            stack->nrepl-value))

(define (exception->nrepl-messages result)
  (let* ((exception (assoc-ref result 'exception-value))
         (stack (assoc-ref result 'stack))
         (error (exception->string exception)))
    `((("err" . ,error)
       ("ares.evaluation/stack" . ,(stack->nrepl-value stack)))
      (("ex" . ,(symbol->string (exception-kind exception)))
       ("status" . #("error" "eval-error" "done")))
      (("status" . #("done"))))))

(define* (evaluation-result->nrepl-messages
          result
          #:key
          (format-value object->pretty-string))
  (let ((result-type (assoc-ref result 'result-type)))
    (case result-type
      ((value)
       `((("value" . ,(format-value (assoc-ref result 'eval-value)))
          ("status" . #("done")))))
      ((multiple-values)
       (multiple-values->nrepl-messages result format-value))
      ((exception)
       (exception->nrepl-messages result))
      ((interrupted)
       `((("status" . #("done" "interrupted")))))
      (else (error (format #f "unknown result-type: ~a\n" result-type))))))

(define (multiple-values->nrepl-messages result format-value)
  "Returns a few nrepl messages with additional status multiple-values,
the last message doesn't contain the value, it contains only
@code{((\"status\" . (\"done\", \"multiple-values\")))}."
  (let lp ((vals (assoc-ref result 'eval-value))
           (msgs '()))
    (if (null? vals)
        (reverse (cons `(("status" . #("done" "multiple-values"))) msgs))
        (lp (cdr vals)
            (cons `(("value" . ,(format-value (car vals)))
                    ("status" . #("multiple-values")))
                  msgs)))))

(define (interrupt-result->nrepl-messages result)
  (define status (assoc-ref result 'status))
  (case status
    ((done)
     `((("status" . #("done" "interrupted")))))
    ((idle)
     `((("status" . #("done" "session-idle")))))))

(define (frame-environment frame)
  "Like (@ (system vm frame) frame-environment) but with a workaround for
a bug with bindings."
  (let* ((frame-num-locals (@@ (system vm frame) frame-num-locals))
         (nlocals (frame-num-locals frame)))
    (fold
     (lambda (binding acc)
       (let* ((slot (binding-slot binding)))
         ;; From frame-call-representation source:
         ;; “HACK: Avoid out-of-range from frame-local-ref.
         ;; Some frames have bindings beyond nlocals.  That
         ;; is probably a bug somewhere else, but at least
         ;; this workaround allows them to be printed.”
         (if (< slot nlocals)
	     (cons
              (cons (binding-name binding) (binding-ref binding))
              acc)
             acc)))
     '()
     (frame-bindings frame))))

(define (frame->nrepl-value frame)
  "Serializes FRAME into a value that can be sent in nREPL messages."
  (define (ensure-list d) (if (list? d) d (list d)))
  (define-syntax fallback
    (syntax-rules ()
      ((fallback default exp)
       (or (with-exception-string-handler
            (lambda (str)
              (format #t "Exception in frame serialization:~%~a" str)
              #f)
            (lambda () exp))
           default))))

  (let ((name (fallback "_"
               (or
                (and=> (frame-procedure-name frame)
                       symbol->string)
                "_")))
        (arguments
         (fallback '()
          (map (lambda (argument)
                 (format #f "~s" argument))
               (ensure-list (frame-arguments frame)))))
        (environment
         (fallback '()
          (map (lambda (binding)
                 (match-let (((name . value) binding))
                   (cons name (format #f "~s" value))))
               (frame-environment frame))))
        (source
         (fallback #f
          (and-let* ((source (frame-source frame)))
            `((line . ,(source:line source))
              (column . ,(source:column source))
              (file . ,(or (and (source:file source) (search-in-load-path (source:file source)))
                           (source:file source))))))))
    `((procedure-name . ,name)
      (arguments . ,(list->vector arguments))
      (environment . ,(list->vector environment))
      (source . ,source))))

(define (stack->nrepl-value stack)
  "Serializes STACK into a value that can be sent in nREPL messages."
  (let ((length (stack-length stack)))
    (cond
     ((< length 0)
      #())
     (else
      (list->vector
       (let loop ((index 0)
                  (frame (stack-ref stack 0))
                  (result '()))
         (if (and frame (< index length))
             (loop (1+ index)
                   (frame-previous frame)
                   (cons (frame->nrepl-value frame) result))
             result)))))))
