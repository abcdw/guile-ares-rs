;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl reporters)
  #:use-module ((ares suitbl definitions) #:select (test? suite?))
  #:use-module ((ares suitbl state) #:prefix state:)
  #:use-module ((ares suitbl reporting)
                #:select (format-location
                          actual pre-evaled-expression
                          string-repeat
                          tests->pretty-string
                          test-reporters-use-all
                          test-reporters-use-first
                          tree-node-children
                          tree-node-description
                          suite-forest->tree-string
                          count-suites-and-tests
                          forest->junit-xml))
  #:use-module ((ares guile exceptions) #:select (exception->string))
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((srfi srfi-197) #:select (chain))

  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((ice-9 match) #:select (match))

  #:export (output-port*
            silent
            logging
            unhandled
            base
            dots
            dots-with-hierarchy
            minimal
            spying
            junit
            tree
            loaded-summary
            run-summary))



;;;
;;; Test Reporters
;;;

#|

Test reporters are simple functions which accept a message in format
of Association List (alist) and produce an output to
output-port*.

(test-reporter
 `((type . reporter/test-loaded)
   (suite-path . ("suite1" "nested-suite"))
   (description . "basic arithmetics")))


Test reporters can be comined with test-reporters-use-all or
test-reporters-use-first to compliment each other or override.

A final test reporter can be attached to test runner.

|#

(define output-port* (make-parameter (current-output-port)))

(define (test-reporters-use-all reporters)
  "Create a reporter, which combines all reporters."
  (lambda (message)
    (for-each (lambda (r) (r message)) reporters)))

(define (test-reporters-use-first reporters)
  "Create a reporter, which uses the first successful reporter."
  (lambda (message)
    (let loop ((reporters reporters))
      (unless (null? reporters)
        (let ((reporter-result ((car reporters) message)))
          (or reporter-result (loop (cdr reporters))))))))

(define (silent message)
  "Do nothing, return @code{#t}."
  #t)

(define (logging message)
  "Just log the @code{message}."
  (format (output-port*) "message: ~y" message))

(define (unhandled message)
  "A simple test reporter, which prints incomming message.  It can be
combined with another reporter using @code{test-reporters-use-first}
to catch unhandled messages."
  (format (output-port*)
          "\nmessage is not handled:\n~y\n" message))

(define (hierarchy message)
  (case (assoc-ref message 'type)
    ((reporter/test-loaded)
     (format (output-port*) "~a"
             (string-repeat "|" (length (assoc-ref message 'suite-path))))
     (format (output-port*) " + test ~a\n"
             (assoc-ref message 'description)))
    ((reporter/suite-enter)
     (format (output-port*) "~a"
             (string-append
              (string-repeat "|" (length (assoc-ref message 'suite-path)))
              "┌"))
     (format (output-port*) "> ~a\n"
             (assoc-ref message 'description)))
    ((reporter/suite-leave)
     (format (output-port*) "~a"
             (string-append
              (string-repeat "|" (length (assoc-ref message 'suite-path)))
              "└"))
     (format (output-port*) "> ~a\n"
             (assoc-ref message 'description)))

    ((reporter/print-suite)
     (format (output-port*) "~y"
             (tests->pretty-string (assoc-ref message 'suite))))
    (else #f)))

(define (verbose message)
  (case (assoc-ref message 'type)
    ((reporter/test-start)
     (format (output-port*) "\n┌Test ~a\n"
             (assoc-ref message 'description)))
    ((reporter/test-end)
     (format (output-port*) "└Test ~a\n"
             (assoc-ref message 'description)))

    ((reporter/assertion-pass)
     (format (output-port*) "~y✓\n"
             (assoc-ref message 'assert/body)))

    ((reporter/assertion-fail)
     (format (output-port*) "~a\n~y✗ ~a\n"
             (format-location message)
             (assoc-ref message 'assert/body) (actual message)))

    ((reporter/assertion-error)
     (format (output-port*) "~a\n~y✗ produced error:\n ~s\n"
             (format-location message)
             (assoc-ref message 'assert/body)
             (exception->string
              (assoc-ref message 'assertion/error))))

    (else #f)))

(define (loading-minimal message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type
    ((reporter/test-loaded)
     (format (output-port*) "-> ~a\n"
             (assoc-ref message 'description)))

    ;; ((reporter/suite-enter)
    ;;  (format (output-port*) "["))
    ;; ((reporter/suite-leave)
    ;;  (format (output-port*) "]"))
    (else #f)))

(define (execution-minimal message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type

    ((reporter/test-start)
     (format (output-port*) "--- [~a] ---\n"
             (assoc-ref message 'description)))
    ((reporter/test-end)
     (format (output-port*) "\n"))

    ((reporter/assertion-pass)
     (format (output-port*) "✓"))

    ((reporter/assertion-fail)
     (format (output-port*) "~a\n~y✗ ~a\n"
             (format-location message)
             (assoc-ref message 'assert/body) (actual message)))

    ((reporter/assertion-error)
     (format (output-port*) "~a\n~y✗ produced error:\n ~s\n"
             (format-location message)
             (assoc-ref message 'assert/body)
             (exception->string
              (assoc-ref message 'assertion/error))))

    (else #f)))

(define minimal
  (chain (list
          execution-minimal
          loading-minimal)
    (test-reporters-use-all _)
    (list _ unhandled)
    (test-reporters-use-first _)))

(define (execution-spying message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type

    ((reporter/test-start)
     (format (output-port*) "--- [~a] ---\n"
             (assoc-ref message 'description)))
    ((reporter/test-end)
     (format (output-port*) "\n"))

    ((reporter/assertion-pass reporter/assertion-fail)
     (format (output-port*) "~y~y => ~y"
             (assoc-ref message 'assert/body)
             (pre-evaled-expression message)
             (assoc-ref message 'assertion/result)))

    ((reporter/assertion-error)
     (format (output-port*) "\n ~a\n ~y✗ produced error:\n ~s\n"
             (format-location message)
             (assoc-ref message 'assert/body)
             (exception->string
              (assoc-ref message 'assertion/error))))

    (else #f)))

(define spying
  (chain (list
          execution-spying
          loading-minimal)
    (test-reporters-use-all _)
    (list _ unhandled)
    (test-reporters-use-first _)))

(define (tree message)
  "A reporter that prints the complete suite tree (like the @code{tree}
CLI command) when a top-level suite finishes loading."
  (case (assoc-ref message 'type)
    ((reporter/suite-tree-loaded)
     (let ((suite-node (assoc-ref message 'suite-node)))
       (format (output-port*) "\n~a"
               (suite-forest->tree-string (list suite-node)))))
    (else #f)))

(define (loaded-summary message)
  "A reporter that prints the number of loaded suites and tests
when a top-level suite finishes loading."
  (case (assoc-ref message 'type)
    ((reporter/suite-tree-loaded)
     (let* ((suite-node (assoc-ref message 'suite-node))
            (counts (count-suites-and-tests suite-node))
            (suites (assoc-ref counts 'suites))
            (tests (assoc-ref counts 'tests))
            (modules (assoc-ref counts 'module-suites))
            (empty (assoc-ref counts 'empty-suites)))
       (format (output-port*)
               "Loaded ~a test~p and ~a suite~p (~a module~p, ~a empty).\n"
               tests tests suites suites modules modules empty)))
    (else #f)))

(define (run-summary message)
  "A reporter that prints a summary line after all tests have been executed."
  (case (assoc-ref message 'type)
    ((reporter/run-end)
     (let ((summary (state:get-run-summary (assoc-ref message 'state))))
       (if summary
           (let ((tests (assoc-ref summary 'tests))
                 (assertions (assoc-ref summary 'assertions))
                 (failures (assoc-ref summary 'failures))
                 (errors (assoc-ref summary 'errors)))
             (format (output-port*)
                     "Ran ~a assertion~p in ~a test~p: ~a failure~p, ~a error~p.\n"
                     assertions assertions
                     tests tests
                     failures failures
                     errors errors))
           (format (output-port*)
                   "No test results available.\n"))))
    (else #f)))

(define base
  (chain (list verbose
               ;; hierarchy
               tree
               loaded-summary
               run-summary)
    (test-reporters-use-all _)
    (list _ unhandled)
    (test-reporters-use-first _)))

(define (dots message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type
    ((reporter/suite-start)
     (format (output-port*) "["))
    ((reporter/suite-end)
     (format (output-port*) "]"))

    ((reporter/test-start)
     (format (output-port*) "("))
    ((reporter/test-end)
     (format (output-port*) ")"))
    ((reporter/test-skip)
     (format (output-port*) "(S)"))

    ((reporter/assertion-pass)
     (format (output-port*) "."))
    ((reporter/assertion-fail)
     (format (output-port*) "F"))
    ((reporter/assertion-error)
     (format (output-port*) "E"))

    (else #f)))

(define dots-with-hierarchy
  (chain (list dots hierarchy)
    (test-reporters-use-all _)
    (list _ unhandled)
    (test-reporters-use-first _)))

(define (junit message)
  "A test reporter that emits JUnit XML to @code{output-port*}
after all tests have finished running.  Silent for all other message types."
  (case (assoc-ref message 'type)
    ((reporter/run-end)
     (let* ((state (assoc-ref message 'state))
            (forest (state:get-suite-forest-with-summary state))
            (xml (forest->junit-xml forest)))
       (format (output-port*) "~a\n" xml)))
    (else #f)))
