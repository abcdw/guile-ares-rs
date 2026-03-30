;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl presets-test)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl runners)
  #:use-module ((ares suitbl runner-state)
                #:select (get-scheduled-tests
                          get-runner-config
                          get-run-history))
  #:use-module ((srfi srfi-1) #:select (lset=))
  #:use-module ((ares suitbl presets)
                #:select (scheduler:slow
                          scheduler:fast
                          make-scheduler:matching
                          scheduler:failed-or-all
                          compose-schedulers

                          preset:only-slow!
                          preset:only-fast!
                          preset:matching!
                          preset:rerun-failed!
                          preset:reset!)))

(define (make-test-runner-with-mixed-tests)
  "Create a runner with a mix of slow and fast tests loaded."
  (define tr (make-silent-test-runner))
  (with-test-runner tr
    (suite "mixed tests"
      (test "fast addition"
        (is (= 4 (+ 2 2))))
      (test "slow network call" 'metadata '((slow? . #t))
        (is #t))
      (test "fast string check"
        (is (string? "hello")))
      (test "slow database query" 'metadata '((slow? . #t))
        (is #t))))
  tr)

(define (runner->state runner)
  (runner `((type . runner/get-state))))

(define (test-descriptions tests)
  "Extract descriptions from a list of test alists."
  (map (lambda (t) (assoc-ref t 'test/description)) tests))

(define (scheduled-descriptions runner)
  "Get the descriptions of all scheduled tests for RUNNER."
  (let ((state (runner->state runner)))
    (test-descriptions
     (get-scheduled-tests state (get-runner-config state)))))


;;;
;;; Scheduler tests
;;;

(define-suite scheduler-tests
  (test "scheduler:slow keeps only slow tests"
    (define tr (make-test-runner-with-mixed-tests))
    (define state (runner->state tr))
    (define slow (scheduler:slow (get-scheduled-tests state '()) state))
    (is (= 2 (length slow)))
    (is (lset= equal?
               '("slow network call" "slow database query")
               (test-descriptions slow))))

  (test "scheduler:fast keeps only fast tests"
    (define tr (make-test-runner-with-mixed-tests))
    (define state (runner->state tr))
    (define fast (scheduler:fast (get-scheduled-tests state '()) state))
    (is (= 2 (length fast)))
    (is (lset= equal?
               '("fast addition" "fast string check")
               (test-descriptions fast))))

  (test "make-scheduler:matching filters by description pattern"
    (define tr (make-test-runner-with-mixed-tests))
    (define state (runner->state tr))
    (define matched
      ((make-scheduler:matching "slow") (get-scheduled-tests state '()) state))
    (is (= 2 (length matched)))
    (is (lset= equal?
               '("slow network call" "slow database query")
               (test-descriptions matched))))

  (test "make-scheduler:matching with specific pattern"
    (define tr (make-test-runner-with-mixed-tests))
    (define state (runner->state tr))
    (define matched
      ((make-scheduler:matching "addition") (get-scheduled-tests state '()) state))
    (is (= 1 (length matched)))
    (is (equal? "fast addition"
                (assoc-ref (car matched) 'test/description))))

  (test "scheduler:failed-or-all returns all tests when none failed"
    (define tr (make-test-runner-with-mixed-tests))
    (define state (runner->state tr))
    (define all-tests (get-scheduled-tests state '()))
    ;; All tests pass, so scheduler:failed-or-all should return all of them
    (define scheduled
      (scheduler:failed-or-all all-tests state))
    (is (= (length all-tests) (length scheduled))))

  (test "scheduler:failed-or-all keeps tests that errored"
    (define tr (make-silent-test-runner))
    (with-test-runner tr
      (suite "suite with failures"
        (test "passing test"
          (is #t))
        (test "failing test"
          (is #f))
        (test "erroring test"
          (is (throw 'boom)))))
    (define state (runner->state tr))
    (define scheduled
      (scheduler:failed-or-all (get-scheduled-tests state '()) state))
    (is (= 2 (length scheduled)))
    (is (lset= equal?
               '("failing test" "erroring test")
               (test-descriptions scheduled))))

  (test "compose-schedulers chains filters"
    (define tr (make-test-runner-with-mixed-tests))
    (define state (runner->state tr))
    (define composed
      (compose-schedulers scheduler:slow (make-scheduler:matching "network")))
    (define result (composed (get-scheduled-tests state '()) state))
    (is (= 1 (length result)))
    (is (equal? "slow network call"
                (assoc-ref (car result) 'test/description))))

  (test "compose-schedulers with no schedulers returns all tests"
    (define tr (make-test-runner-with-mixed-tests))
    (define state (runner->state tr))
    (define composed (compose-schedulers))
    (define all-tests (get-scheduled-tests state '()))
    (define result (composed all-tests state))
    (is (= (length all-tests) (length result)))))


;;;
;;; Preset tests
;;;

(define-suite preset-tests
  (test "preset:only-slow! configures runner for slow tests"
    (define tr (make-test-runner-with-mixed-tests))
    (preset:only-slow! tr)
    (is (lset= equal?
               '("slow network call" "slow database query")
               (scheduled-descriptions tr))))

  (test "preset:only-fast! configures runner for fast tests"
    (define tr (make-test-runner-with-mixed-tests))
    (preset:only-fast! tr)
    (is (lset= equal?
               '("fast addition" "fast string check")
               (scheduled-descriptions tr))))

  (test "preset:matching! configures runner with pattern filter"
    (define tr (make-test-runner-with-mixed-tests))
    (preset:matching! "database" tr)
    (is (equal? '("slow database query")
                (scheduled-descriptions tr))))

  (test "preset:rerun-failed! configures runner for failed tests"
    (define tr (make-silent-test-runner))
    (with-test-runner tr
      (suite "suite with a failure"
        (test "good test"
          (is #t))
        (test "bad test"
          (is #f))))
    (preset:rerun-failed! tr)
    (is (equal? '("bad test")
                (scheduled-descriptions tr))))

  (test "preset:reset! restores default scheduling"
    (define tr (make-test-runner-with-mixed-tests))
    (preset:only-slow! tr)
    (is (= 2 (length (scheduled-descriptions tr))))
    (preset:reset! tr)
    (is (= 4 (length (scheduled-descriptions tr)))))

  (test "presets default to current test-runner*"
    (define tr (make-test-runner-with-mixed-tests))
    (with-test-runner tr
      (preset:only-slow!)
      (is (lset= equal?
                 '("slow network call" "slow database query")
                 (scheduled-descriptions tr)))
      (preset:reset!)
      (is (= 4 (length (scheduled-descriptions tr)))))))
