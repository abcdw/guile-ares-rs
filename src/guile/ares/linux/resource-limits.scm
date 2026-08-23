;;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (ares linux resource-limits)
  #:use-module (ice-9 format)
  #:export (current-rttime-limit
            warn-if-zero-rttime-limit!))


;;;
;;; Linux real-time CPU limit
;;;

;; RLIMIT_RTTIME is Linux resource 15.  Do not use Guile's 'rttime
;; symbol here: through Guile 3.0.11 it resolves to the wrong resource.
(define %rlimit-rttime 15)

(define (current-rttime-limit)
  "Return the soft Linux RLIMIT_RTTIME limit, or false when unlimited."
  (call-with-values
      (lambda ()
        (getrlimit %rlimit-rttime))
    (lambda (soft-limit hard-limit)
      soft-limit)))

(define (warn-if-zero-rttime-limit!)
  "Warn when the process has a zero real-time CPU limit."
  (let ((limit (current-rttime-limit)))
    (when (and limit (zero? limit))
      (format (current-warning-port)
              "\
=====================================================================
warning: Ares is starting with RLIMIT_RTTIME set to zero.
Fibers preemption can cause the kernel to kill Ares.
Possible cause is pipewire:
~a.
=====================================================================
"
              "\
https://lists.sr.ht/~abcdw/rde-discuss/%3C87ik52z856.fsf@trop.in%3E"))))
