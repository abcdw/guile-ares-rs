;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl exceptions)
  #:use-module (ice-9 exceptions)
  #:export (&suitbl-wrong-position-exception
            make-suitbl-wrong-position-exception
            suitbl-wrong-position-exception?
            suitbl-wrong-position-exception-form
            suitbl-wrong-position-exception-position
            raise-suitbl-wrong-position-exception))


;;;
;;; Exceptions
;;;

(define-exception-type
  &suitbl-wrong-position-exception &exception
  make-suitbl-wrong-position-exception
  suitbl-wrong-position-exception?
  (form suitbl-wrong-position-exception-form)
  (position suitbl-wrong-position-exception-position))

(define* (raise-suitbl-wrong-position-exception form position
                                                #:optional message)
  (raise-exception
   (make-exception
    (make-suitbl-wrong-position-exception form position)
    (make-exception-with-message
     (or message
         (format #f "~a used in wrong position: ~a" form position))))))
