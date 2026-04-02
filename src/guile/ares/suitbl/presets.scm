;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl presets)
  #:use-module ((ares suitbl definitions) #:select (test-runner*))
  #:use-module ((ares suitbl runner-state)
                #:select (set-runner-config-value!
                          get-runner-config))
  #:use-module ((ares suitbl schedulers)
                #:prefix scheduler:)

  #:export (only-slow!
            only-fast!
            matching!
            rerun-failed-or-all!
            raise-on-error!
            reset!))


;;;
;;; Helpers
;;;

(define* (runner->state #:optional (runner (test-runner*)))
  (runner `((type . runner/get-state))))



;;;
;;; Presets
;;;

(define* (only-slow! #:optional (runner (test-runner*)))
  "Configure RUNNER to schedule only slow tests."
  (let ((state (runner->state runner)))
    (set-runner-config-value! state 'schedule-tests scheduler:slow)
    (get-runner-config state)))

(define* (only-fast! #:optional (runner (test-runner*)))
  "Configure RUNNER to schedule only fast tests."
  (let ((state (runner->state runner)))
    (set-runner-config-value! state 'schedule-tests scheduler:fast)
    (get-runner-config state)))

(define* (matching! pattern #:optional (runner (test-runner*)))
  "Configure RUNNER to schedule only tests matching regexp PATTERN."
  (let ((state (runner->state runner)))
    (set-runner-config-value! state 'schedule-tests (scheduler:make-matching pattern))
    (get-runner-config state)))

(define* (rerun-failed-or-all! #:optional (runner (test-runner*)))
  "Configure RUNNER to schedule only tests that failed or errored
in the previous run."
  (let ((state (runner->state runner)))
    (set-runner-config-value! state 'schedule-tests scheduler:failed-or-all)
    (get-runner-config state)))

(define* (raise-on-error! #:optional (runner (test-runner*)))
  "Configure RUNNER to re-raise exceptions on assertion failures and
errors without unwinding the stack, so the IDE can bring up a stack
trace viewer."
  (let ((state (runner->state runner)))
    (set-runner-config-value! state 're-raise? #t)
    (get-runner-config state)))

(define* (reset! #:optional (runner (test-runner*)))
  "Remove the schedule-tests filter from RUNNER, restoring default
behavior of running all loaded tests."
  (let ((state (runner->state runner)))
    (set-runner-config-value! state 'schedule-tests scheduler:all)
    (set-runner-config-value! state 're-raise? #f)
    (get-runner-config state)))

(define (comment)
  (reset!)
  (rerun-failed-or-all!)
  (raise-on-error!)
  )
