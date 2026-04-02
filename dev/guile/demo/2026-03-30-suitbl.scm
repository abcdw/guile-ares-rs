;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2026 Andrew Tropin <andrew@trop.in>
;; SUITBL Demo — Scheme Universal Interactive Testing Base Library
;;
;; Evaluate forms inside (comment ...) blocks interactively in your
;; REPL / Arei to explore the API.  Nothing runs on module load.

(define-module (demo 2026-03-30-suitbl)
  #:use-module (ares atomic)
  #:use-module (ares guile prelude)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl runners)
  #:use-module ((ares suitbl runner-state) #:prefix state:)
  #:use-module (ares suitbl reporters)
  #:use-module ((ares suitbl presets) #:prefix preset:))


;;;
;;; 0. Portable Test Definition API
;;;

(comment
 (display "hello")
 (+ 2 2)

 (is 42)
 (is (= 2 3))

 (test "combine"
   (define some-setup #f)
   (is 42)
   (is some-setup))

 (suite "math operations"
   (suite "empty sweet with metadata"
     'metadata '((tags . (integration)))
     "hello")

   (test "addition"
     (is (= 3 (+ 1 2)))
     (is (= 0 (+ -1 1))))

   ;; Test with metadata <--------------------------------------
   (test "multiplication"
     'metadata
     '((slow? . #t))
     (is (= 12 (* 3 4)))
     (is (= 0 (* 0 999))))

   (test "equality"
     (is (= 2 3))
     (is (= 2 (/ 4 2))))

   (test "inequality"
     (is (= 2 3))
     (is (= 2 (/ 4 0))))

   (suite "edge cases"
     (test "division by large numbers"
       (is (= 0 (floor (/ 1 1000000)))))))

 (suite-thunk "some fancy tests"
   (test "1"
     'hey)
   (test "2"
     'hehey))

 (define-suite some-cool-tests
   (suite "inner suite"
     (test "1"
       (is #t)))
   (test "cool"
     (is #f)))
 (some-cool-tests)

 test-runner*
 (test-runner*)

 )


#|
Well-defined terminology for all primary testing entities.
It's easy to communicate and reason about.

Entities are runtime friendly.

Integrate well in interactive workflows (REPL-driven)
(Still easy to use from CLI)

Definition is completely decoupled from execution.
Full control of loading and execution behavior
- Parallel execution
- Ordering, filtering
- Re-runing only failed
- Fixture setup
- Guarding from continuations and exceptions
- etc

Full control of reporting

API-first, can be easily integrated in your IDE and other tools.
- Test discovery
- Automatic test rerun on eval
- Bring debugger on error
- Any combination of

|#

(comment

 ;; 1. Load project tests without executing them.

 ;; Run tests

 ;; Run only slow tests
 (preset:only-slow!)

 ;; Run only tests matching a regexp
 (preset:matching! "macro")

 ;; Re-run only tests that failed last time
 (preset:rerun-failed-or-all!)

 ;; Re-raise exceptions for stack trace debugging in IDE
 (preset:raise-on-error!)

 ;; Back to defaults
 (preset:reset!))

#|
- is, test, suite, suite-thunk, define-suite
- Metadata

- Test loading, scheduling and execution
- Test runners message API
- Test runners customization
- Custom test reporters

- IDE integration
- Debbugger on error

- Actually fixing failing tests
|#



;;;
;;; 1. Assertions with `is` and test runner messages
;;;

;; `is` is the fundamental assert macro.  It comes in two flavors:
;; - (is expr)           - asserts expr is truthy
;; - (is (pred args...)) - asserts predicate holds; on failure the
;;                          args are evaluated separately for a clear
;;                          error message.

(comment
 (test-runner* (make-suitbl-test-runner
                #:config '((log-runner-messages? . #t))))

 ;; Simple truthy assertion
 (is #t)
 (is 42)
 (is (= 2 (+ 3 2)))

 ((test-runner*)
  `((type . runner/get-log)))

 (begin
   (is #t)
   (is 42)
   (is "non-empty strings are truthy too"))

 ;; Predicate form
 (is (= 4 (+ 2 2)))
 (is (equal? '(1 2 3) (list 1 2 3)))

 ;; Meaningful reporters
 (is (string=? "hellou" (string-append "hel" "lo")))
 (is (= 5 (+ 2 2)))

 )


;;;
;;; 2. Loading tests and test runner customization
;;;

;; `test` groups related assertions into a named test case.

(define (runner-get-state-pretty)
  (chain ((test-runner*) `((type . runner/get-state)))
    (atomic-box-ref _)
    (format #t "~y" _)))

(comment
 (test-runner*)
 (test-runner* (make-suitbl-test-runner
                #:config '((log-runner-messages? . #t))))
 (runner-get-state-pretty)

 (test "basic arithmetic"
   (is (= 2 (+ 1 1)))
   (is (= 6 (* 2 3)))
   (is (= 0 (- 5 5))))

 ((test-runner*) `((type . runner/get-log)))
 (runner-get-state-pretty)

 ;;; No auto-run

 (test-runner* (make-suitbl-test-runner
                #:config '((log-runner-messages? . #t)
                           (auto-run? . #f))))

 (test "string operations"
   (is (string=? "ABC" (string-upcase "abc")))
   (is (= 5 (string-length "hello"))))


 ((test-runner*) `((type . runner/get-log)))
 ((test-runner*) `((type . runner/run-tests)))

 (runner-get-state-pretty)

 )


;;;
;;; 3. Hierarchical grouping and scheduling basics
;;;

;; `suite` groups tests (and nested suites) under a named umbrella.
;; The tree reporter prints a nice hierarchy when loading completes.

(comment
 (test-runner* (make-suitbl-test-runner
                #:config '((log-runner-messages? . #t))))

 (suite "math operations"
   (suite "empty one"
     'hey)
   (test "addition"
     (is (= 3 (+ 1 2)))
     (is (= 0 (+ -1 1))))

   (test "multiplication"
     'metadata
     '((slow? . #t))
     (is (= 12 (* 3 4)))
     (is (= 0 (* 0 999))))

   (test "equality"
     (is (= 2 2))
     (is (= 2 (/ 4 2))))

   (suite "edge cases"
     (test "division by large numbers"
       (is (= 0 (floor (/ 1 1000000))))))))


(comment
 (chain ((test-runner*) `((type . runner/get-log)))
   (map (lambda (m) (assoc-ref m 'type)) _))

 (chain ((test-runner*) `((type . runner/get-state)))
   (state:get-run-history _)
   (state:simplify-run-history _))

 (chain ((test-runner*) `((type . runner/get-log)))
   (map (lambda (m) (assoc-ref m 'type)) _))

 ((test-runner*) `((type . runner/run-tests)))

 ((test-runner*) `((type . runner/run-tests)
                   (runner/config
                    . ((schedule-tests . ,(lambda (tests state)
                                            (reverse tests))))))))

;;;
;;; 4. `define-suite` - reusable named suites
;;;

;; `define-suite` is equivalent to (define-public NAME (suite-thunk ...)).
;; The suite thunk can be called repeatedly to re-run.

(define-suite list-tests
  (test "cons builds pairs"
    (is (equal? '(1 . 2) (cons 1 2)))
    (is (equal? '(1 2 3) (cons 1 '(2 3)))))

  (test "map transforms lists"
    (is (equal? '(2 4 6) (map (lambda (x) (* 2 x)) '(1 2 3))))))

(comment
 ;; Re-run the suite by calling the thunk
 (list-tests))


;;;
;;; 5. Metadata on tests and suites
;;;

;; Both `test` and `suite` accept an optional 'metadata alist.
;; Schedulers and reporters can use metadata to filter or annotate.

(comment
 (suite "tagged suite" 'metadata '((tags . (integration)))
   (test "fast check"
     (is (= 1 1)))

   (test "slow check" 'metadata '((slow? . #t))
         (is (= 2 (+ 1 1)))))

 ((test-runner*) `((type . runner/run-tests)))

 ;; Re-run, but skip slow tests via inline config
 ((test-runner*) `((type . runner/run-tests)
                   (runner/config
                    . ((schedule-tests . ,scheduler:fast))))))

;;;
;;; 6. Custom reporters
;;;

;; Reporters are simple procedures (message -> output).  SUITBL ships
;; several and lets you compose them.

(comment
 ;; Dots reporter — compact output
 (with-test-runner
  (make-suitbl-test-runner
   #:config `((test-reporter . ,test-reporter-dots)))
  (suite "dots demo"
    (test "all pass" (is #t) (is #t) (is #t))
    (test "one fails" (is #t) (is #f))))

 ;; Minimal reporter — test names + pass/fail marks
 (with-test-runner
  (make-suitbl-test-runner
   #:config `((test-reporter . ,test-reporter-minimal)))
  (suite "hehe"
    (test "minimal demo"
      (is (= 1 1))
      (is (= 1 1))
      (is (= 1 1))
      (is (= 2 2)))
    (test "minimal demo 2"
      (is (= 1 1))
      (is (= 2 2)))))

 ;; Compose reporters: tree structure + dots
 (with-test-runner
  (make-suitbl-test-runner
   #:config `((test-reporter
               . ,(test-reporters-use-all
                   (list test-reporter-tree
                         test-reporter-dots)))))
  (suite "composed reporters"
    (test "a" (is #t))
    (test "b" (is #t)))))


;;;
;;; 7. Silent runner - capture results without output
;;;

(comment
 (with-test-runner
  (make-suitbl-test-runner
   #:config `((test-reporter . ,test-reporter-silent)))
  (suite "quiet suite"
    (test "no output" (is #t) (is #t)))

  ;; Retrieve the run summary programmatically
  ((test-runner*) `((type . runner/get-run-summary))))

 )


;;;
;;; 8. Presets and integration with IDE
;;;

;; Presets modify the current test runner's scheduler/config in place.
;; Great for interactive workflows: focus on what matters right now.

(comment
  ((test-runner*) `((type . runner/run-tests)))

 ;; Run only slow tests
 (preset:only-slow!)

 ;; Run only tests matching a regexp
 (preset:matching! "macro")

 ;; Re-run only tests that failed last time
 (preset:rerun-failed-or-all!)

 ;; Re-raise exceptions for stack trace debugging in IDE
 (preset:raise-on-error!)

 ;; Back to defaults
 (preset:reset!))
