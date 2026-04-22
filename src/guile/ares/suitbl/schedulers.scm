;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl schedulers)
  #:use-module ((ares suitbl state)
                #:select (get-run-history))
  #:use-module ((srfi srfi-1) #:select (filter filter-map))
  #:use-module ((srfi srfi-197) #:select (chain-and))

  #:export (all
            slow
            fast
            non-dev
            make-matching
            make-module
            failed-or-all
            compose))

;;;
;;; Schedulers
;;;
;;; A scheduler is a procedure with the signature (tests state) -> tests,
;;; where STATE is the runner's atomic-box state.  This allows schedulers
;;; to query the runner (e.g. run history) at scheduling time.
;;;

(define (all tests state)
  "Default scheduler that keeps all tests."
  tests)

(define (test-metadata test)
  (or (assoc-ref test 'test/compound-metadata)
      (assoc-ref test 'test/metadata)
      '()))

(define (slow tests state)
  "Keep only tests with @code{(slow? . #t)} in compound metadata."
  (filter (lambda (t)
            (assoc-ref (test-metadata t) 'slow?))
          tests))

(define (fast tests state)
  "Keep only tests without @code{slow?} in compound metadata."
  (filter (lambda (t)
            (not (assoc-ref (test-metadata t) 'slow?)))
          tests))

(define (non-dev tests state)
  "Keep only tests without @code{(dev? . #t)} in compound metadata."
  (filter (lambda (t)
            (not (assoc-ref (test-metadata t) 'dev?)))
          tests))

(define (make-matching pattern)
  "Return a scheduler that keeps tests whose description matches
a regexp PATTERN."
  (define rx (make-regexp pattern))
  (lambda (tests state)
    (filter (lambda (t)
              (let ((description (or (assoc-ref t 'test/description) "")))
                (regexp-exec rx description)))
            tests)))

(define (make-module pattern)
  "Return a scheduler that keeps tests whose module name matches
a regexp PATTERN."
  (define rx (make-regexp pattern))
  (lambda (tests state)
    (filter (lambda (t)
              (chain-and (test-metadata t)
                (assoc-ref _ 'module)
                (module-name _)
                (format #f "~a" _)
                (regexp-exec rx _)))
            tests)))

(define (failed-or-all tests state)
  "Keep only tests that failed or errored in the previous run.
Reads the current run history from STATE at scheduling time.  If there
are no failures, return all tests unfiltered."
  (let* ((run-history (or (get-run-history state) '()))
         (failed-set
          (filter-map (lambda (entry)
                        (let ((outcome (assoc-ref entry 'test-run/outcome)))
                          (and (memq outcome '(fail error))
                               (assoc-ref entry 'test))))
                      run-history)))
    (if (null? failed-set)
        tests
        (filter (lambda (t) (memq t failed-set)) tests))))

(define (compose . schedulers)
  "Compose SCHEDULERS sequentially, applying each filter in order."
  (lambda (tests state)
    (let loop ((remaining schedulers)
               (result tests))
      (if (null? remaining)
          result
          (loop (cdr remaining)
                ((car remaining) result state))))))
