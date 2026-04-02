;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl presets-test)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl runners)
  #:use-module ((ares suitbl test-utils)
                #:select (make-test-runner-with-mixed-tests
                          scheduled-descriptions))
  #:use-module ((srfi srfi-1) #:select (lset=))
  #:use-module ((ares suitbl presets)
                #:select (preset:only-slow!
                          preset:only-fast!
                          preset:matching!
                          preset:rerun-failed-or-all!
                          preset:reset!)))



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

  (test "preset:rerunfa-iled! configures runner for failed tests"
    (define tr (make-silent-test-runner))
    (with-test-runner tr
      (suite "suite with a failure"
        (test "good test"
          (is #t))
        (test "bad test"
          (is #f))))
    (preset:rerun-failed-or-all! tr)
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
