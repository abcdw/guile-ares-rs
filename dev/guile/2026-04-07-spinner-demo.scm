;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

;; Spinner Demo — terminal braille spinner with configurable duration.

(define-module (demo 2026-04-07-spinner-demo)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 format)
  #:use-module (ares guile prelude))


(define spinner-chars
  (list->vector '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")))

(define (run-spinner duration-ms)
  (let* ((n (vector-length spinner-chars))
         (steps (inexact->exact (round (/ (* duration-ms 1000) 100000)))))
    (let loop ((i 0))
      (when (< i steps)
        (format #t "\r~a " (vector-ref spinner-chars (modulo i n)))
        (force-output)
        (usleep 100000)
        (loop (+ i 1)))))
  (display "\r  \r")
  (force-output))

(comment
 (run-spinner 3000)
 (display "Done!\n"))
