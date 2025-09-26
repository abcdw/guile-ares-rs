;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023, 2024, 2025 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (ares guile prelude)
  #:use-module (ares guile alist)
  #:use-module (srfi srfi-197)
  #:use-module (ares guile exceptions)
  #:re-export (alist-select-keys
               alist-cons
               alist-delete
               chain chain-and
               exception->string)
  #:export (comment))

(define-syntax comment
  (lambda (stx)
    "Ignores all the forms and just returns a @code{#<unspecified>}.  It's
helpful to prevent the evaluation of expressions, while keeping them
around."
    *unspecified*))
