;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023, 2024, 2025 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (ares guile alist)
  #:use-module (srfi srfi-1)
  #:export (alist-get-in alist-select-keys)
  #:re-export (alist-cons alist-delete))

(define (alist-get-in path alist)
  (if (null? path)
      alist
      (alist-get-in (cdr path) (assoc-ref alist (car path)))))

(define (alist-select-keys keys alist)
  "Select entries with keys equal to @code{keys} and return a new alist
with those entries. The order of entries corresponds to order of @code{keys}."
  (fold-right
   (lambda (key acc)
     (let ((entry (assoc key alist)))
       (if entry
           (cons entry acc)
           acc)))
   '()
   keys))
