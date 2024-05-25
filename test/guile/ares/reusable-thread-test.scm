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

(define-module (ares reusable-thread-test)
  #:use-module (ares reusable-thread)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (test-utils))

(define-test test-reusable-thread
  (test-group "Testing reusable thread"
    (test-group "Basic use case: start, reuse, get channel value, shutdown"
      (let* ((rth (make-reusable-thread))
             (th (reusable-thread-thread rth)))

        (reusable-thread-discard-and-run rth (lambda () 'hello))
        (test-equal "Obtain value from thread"
          `((action . return-value) (value . hello))
          (reusable-thread-get-value rth))

        (test-equal "Interrupt idle process"
          '(((action . interrupt) (status . idle)))
          (reusable-thread-interrupt rth))

        (reusable-thread-discard-and-run rth (lambda () 'hello2 (sleep 10)))
        ;; Trying to interrupt finished execution.
        (test-equal "Discard current execution and reuse thread"
          '()
          (reusable-thread-discard-and-run
           rth (lambda () 'hello3 (sleep 10))))

        (test-equal "Interrupt long-running process"
          '(((action . interrupt) (status . done)))
          (reusable-thread-interrupt rth))

        (reusable-thread-shutdown rth)
        ;; after join-thread thread-exited? still returns #f
        (usleep 1)
        (test-assert "Thread exited" (thread-exited? th))))))
