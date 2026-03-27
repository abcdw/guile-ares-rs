;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl presets)
  #:use-module ((ares suitbl definitions) #:select (test-runner*))
  #:use-module ((ares suitbl runner-state)
                #:select (set-runner-config-value!
                          get-run-history))
  #:use-module ((srfi srfi-1) #:select (filter any))

  #:export (scheduler:slow
            scheduler:fast
            scheduler:matching
            scheduler:failed
            compose-schedulers

            preset:only-slow!
            preset:only-fast!
            preset:matching!
            preset:rerun-failed!
            preset:reset!))


;;;
;;; Helpers
;;;

(define* (runner->state #:optional (runner (test-runner*)))
  (runner `((type . runner/get-state))))


;;;
;;; Schedulers
;;;

(define (scheduler:slow tests)
  "Keep only tests with @code{(slow? . #t)} in metadata."
  (filter (lambda (t)
            (let ((metadata (or (assoc-ref t 'test/metadata) '())))
              (assoc-ref metadata 'slow?)))
          tests))

(define (scheduler:fast tests)
  "Keep only tests without @code{slow?} metadata."
  (filter (lambda (t)
            (let ((metadata (or (assoc-ref t 'test/metadata) '())))
              (not (assoc-ref metadata 'slow?))))
          tests))

(define (scheduler:matching pattern)
  "Return a scheduler that keeps tests whose description matches
a regexp PATTERN."
  (define rx (make-regexp pattern))
  (lambda (tests)
    (filter (lambda (t)
              (let ((description (or (assoc-ref t 'test/description) "")))
                (regexp-exec rx description)))
            tests)))

(define (scheduler:failed run-history)
  "Return a scheduler that keeps only tests that failed or errored
in the given RUN-HISTORY."
  (lambda (tests)
    (filter (lambda (t)
              (any (lambda (entry)
                     (and (equal? (assoc-ref entry 'test) t)
                          (let ((result (assoc-ref entry 'test-run/result)))
                            (or (> (assoc-ref result 'failures) 0)
                                (> (assoc-ref result 'errors) 0)))))
                   run-history))
            tests)))

(define (compose-schedulers . schedulers)
  "Compose SCHEDULERS sequentially, applying each filter in order."
  (lambda (tests)
    (let loop ((remaining schedulers)
               (result tests))
      (if (null? remaining)
          result
          (loop (cdr remaining)
                ((car remaining) result))))))


;;;
;;; Presets
;;;

(define* (preset:only-slow! #:optional (runner (test-runner*)))
  "Configure RUNNER to schedule only slow tests."
  (set-runner-config-value!
   (runner->state runner) 'schedule-tests scheduler:slow))

(define* (preset:only-fast! #:optional (runner (test-runner*)))
  "Configure RUNNER to schedule only fast tests."
  (set-runner-config-value!
   (runner->state runner) 'schedule-tests scheduler:fast))

(define* (preset:matching! pattern #:optional (runner (test-runner*)))
  "Configure RUNNER to schedule only tests matching regexp PATTERN."
  (set-runner-config-value!
   (runner->state runner) 'schedule-tests (scheduler:matching pattern)))

(define* (preset:rerun-failed! #:optional (runner (test-runner*)))
  "Configure RUNNER to schedule only tests that failed or errored
in the previous run."
  (let* ((state (runner->state runner))
         (run-history (or (get-run-history state) '())))
    (set-runner-config-value!
     state 'schedule-tests (scheduler:failed run-history))))

(define* (preset:reset! #:optional (runner (test-runner*)))
  "Remove the schedule-tests filter from RUNNER, restoring default
behavior of running all loaded tests."
  (set-runner-config-value!
   (runner->state runner) 'schedule-tests identity))

(define (comment)
  (preset:reset!)
  (preset:rerun-failed!)
 )
