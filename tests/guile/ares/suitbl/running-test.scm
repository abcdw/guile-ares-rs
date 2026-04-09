;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl running-test)
  #:use-module (ares suitbl core)
  #:use-module ((ares suitbl running) #:prefix running:))



(define-suite summarize-test-run-events-tests
  (test "summarize pass-only events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 0)
           (skipped . 0)
           (assertions . 2))
         (running:summarize-test-run-events '(pass pass)))))

  (test "summarize fail-only events"
    (is (equal?
         '((tests . 1)
           (failures . 1)
           (errors . 0)
           (skipped . 0)
           (assertions . 1))
         (running:summarize-test-run-events '(fail)))))

  (test "summarize error-only events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 1)
           (skipped . 0)
           (assertions . 1))
         (running:summarize-test-run-events '(error)))))

  (test "summarize mixed fail and error events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 1)
           (skipped . 0)
           (assertions . 3))
         (running:summarize-test-run-events '(pass fail error)))))

  (test "summarize empty events"
    (is (equal?
         '((tests . 1)
           (failures . 0)
           (errors . 0)
           (skipped . 0)
           (assertions . 0))
         (running:summarize-test-run-events '())))))
