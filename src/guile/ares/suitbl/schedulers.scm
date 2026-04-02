;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl schedulers)
  #:use-module ((ares suitbl runner-state)
                #:select (get-run-history))
  #:use-module ((srfi srfi-1) #:select (filter filter-map))

  #:export (scheduler:all
            scheduler:slow
            scheduler:fast
            make-scheduler:matching
            scheduler:failed-or-all
            compose-schedulers))

;;;
;;; Schedulers
;;;
;;; A scheduler is a procedure with the signature (tests state) -> tests,
;;; where STATE is the runner's atomic-box state.  This allows schedulers
;;; to query the runner (e.g. run history) at scheduling time.
;;;

(define (scheduler:all tests state)
  "Default scheduler that keeps all tests."
  tests)

(define (scheduler:slow tests state)
  "Keep only tests with @code{(slow? . #t)} in metadata."
  (filter (lambda (t)
            (let ((metadata (or (assoc-ref t 'test/metadata) '())))
              (assoc-ref metadata 'slow?)))
          tests))

(define (scheduler:fast tests state)
  "Keep only tests without @code{slow?} metadata."
  (filter (lambda (t)
            (let ((metadata (or (assoc-ref t 'test/metadata) '())))
              (not (assoc-ref metadata 'slow?))))
          tests))

(define (make-scheduler:matching pattern)
  "Return a scheduler that keeps tests whose description matches
a regexp PATTERN."
  (define rx (make-regexp pattern))
  (lambda (tests state)
    (filter (lambda (t)
              (let ((description (or (assoc-ref t 'test/description) "")))
                (regexp-exec rx description)))
            tests)))

(define (scheduler:failed-or-all tests state)
  "Keep only tests that failed or errored in the previous run.
Reads the current run history from STATE at scheduling time.  If there
are no failures, return all tests unfiltered."
  (let* ((run-history (or (get-run-history state) '()))
         (failed-set
          (filter-map (lambda (entry)
                        (let ((result (assoc-ref entry 'test-run/result)))
                          (and (or (> (assoc-ref result 'failures) 0)
                                   (> (assoc-ref result 'errors) 0))
                               (assoc-ref entry 'test))))
                      run-history)))
    (if (null? failed-set)
        tests
        (filter (lambda (t) (memq t failed-set)) tests))))

(define (compose-schedulers . schedulers)
  "Compose SCHEDULERS sequentially, applying each filter in order."
  (lambda (tests state)
    (let loop ((remaining schedulers)
               (result tests))
      (if (null? remaining)
          result
          (loop (cdr remaining)
                ((car remaining) result state))))))
