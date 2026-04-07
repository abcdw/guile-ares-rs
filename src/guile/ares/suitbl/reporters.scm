;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl reporters)
  #:use-module ((ares suitbl definitions) #:select (test? suite?))
  #:use-module ((ares suitbl state) #:prefix state:)
  #:use-module ((ares suitbl reporting)
                #:select (format-location
                          actual pre-evaled-expression
                          string-repeat
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

  #:export (reporter-every
            reporter-first
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
;;; Reporter Combinators
;;;

(define (reporter-every reporters)
  "Create a reporter, which combines all reporters."
  (lambda (message)
    (for-each (lambda (r) (r message)) reporters)))

(define (reporter-first reporters)
  "Create a reporter, which uses the first successful reporter."
  (lambda (message)
    (let loop ((reporters reporters))
      (unless (null? reporters)
        (let ((reporter-result ((car reporters) message)))
          (or reporter-result (loop (cdr reporters))))))))


;;;
;;; Test Reporters
;;;

#|

Test reporters are simple functions which accept a message in format
of Association List (alist) and produce an output to the port
specified via @code{reporting/port} key in the message, falling back
to @code{(current-output-port)}.

(test-reporter
 `((type . load/test)
   (suite-path . ("suite1" "nested-suite"))
   (description . "basic arithmetics")))


Test reporters can be combined with reporter-every or
reporter-first to complement each other or override.

A final test reporter can be attached to test runner.

|#

(define (get-port message)
  (or (assoc-ref message 'reporting/port) (current-output-port)))

(define (silent message)
  "Do nothing, return @code{#t}."
  #t)

(define (logging message)
  "Just log the @code{message}."
  (format (get-port message) "message: ~y" message))

(define (unhandled message)
  "A simple test reporter, which prints incomming message.  It can be
combined with another reporter using @code{reporter-first}
to catch unhandled messages."
  (format (get-port message)
          "\nmessage is not handled:\n~y\n" message))

(define (hierarchy message)
  (case (assoc-ref message 'type)
    ((load/test)
     (format (get-port message) "~a"
             (string-repeat "|" (length (assoc-ref message 'suite-path))))
     (format (get-port message) " + test ~a\n"
             (assoc-ref message 'description)))
    ((load/suite-enter)
     (format (get-port message) "~a"
             (string-append
              (string-repeat "|" (length (assoc-ref message 'suite-path)))
              "┌"))
     (format (get-port message) "> ~a\n"
             (assoc-ref message 'description)))
    ((load/suite-leave)
     (format (get-port message) "~a"
             (string-append
              (string-repeat "|" (length (assoc-ref message 'suite-path)))
              "└"))
     (format (get-port message) "> ~a\n"
             (assoc-ref message 'description)))

    (else #f)))

(define (verbose message)
  (case (assoc-ref message 'type)
    ((run/test-start)
     (format (get-port message) "\n┌Test ~a\n"
             (assoc-ref message 'description)))
    ((run/test-end)
     (format (get-port message) "└Test ~a\n"
             (assoc-ref message 'description)))

    ((run/assertion-pass)
     (format (get-port message) "~y✓\n"
             (assoc-ref message 'assert/body)))

    ((run/assertion-fail)
     (format (get-port message) "~a\n~y✗ ~a\n"
             (format-location message)
             (assoc-ref message 'assert/body) (actual message)))

    ((run/assertion-error)
     (format (get-port message) "~a\n~y✗ produced error:\n ~s\n"
             (format-location message)
             (assoc-ref message 'assert/body)
             (exception->string
              (assoc-ref message 'assertion/error))))

    (else #f)))

(define (loading-minimal message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type
    ((load/test)
     (format (get-port message) "-> ~a\n"
             (assoc-ref message 'description)))

    ;; ((reporter/suite-enter)
    ;;  (format (output-port*) "["))
    ;; ((reporter/suite-leave)
    ;;  (format (output-port*) "]"))
    (else #f)))

(define (execution-minimal message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type

    ((run/test-start)
     (format (get-port message) "--- [~a] ---\n"
             (assoc-ref message 'description)))
    ((run/test-end)
     (format (get-port message) "\n"))

    ((run/assertion-pass)
     (format (get-port message) "✓"))

    ((run/assertion-fail)
     (format (get-port message) "~a\n~y✗ ~a\n"
             (format-location message)
             (assoc-ref message 'assert/body) (actual message)))

    ((run/assertion-error)
     (format (get-port message) "~a\n~y✗ produced error:\n ~s\n"
             (format-location message)
             (assoc-ref message 'assert/body)
             (exception->string
              (assoc-ref message 'assertion/error))))

    (else #f)))

(define minimal
  (chain (list
          execution-minimal
          loading-minimal)
    (reporter-every _)
    (list _ unhandled)
    (reporter-first _)))

(define (execution-spying message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type

    ((run/test-start)
     (format (get-port message) "--- [~a] ---\n"
             (assoc-ref message 'description)))
    ((run/test-end)
     (format (get-port message) "\n"))

    ((run/assertion-pass run/assertion-fail)
     (format (get-port message) "~y~y => ~y"
             (assoc-ref message 'assert/body)
             (pre-evaled-expression message)
             (assoc-ref message 'assertion/result)))

    ((run/assertion-error)
     (format (get-port message) "\n ~a\n ~y✗ produced error:\n ~s\n"
             (format-location message)
             (assoc-ref message 'assert/body)
             (exception->string
              (assoc-ref message 'assertion/error))))

    (else #f)))

(define spying
  (chain (list
          execution-spying
          loading-minimal)
    (reporter-every _)
    (list _ unhandled)
    (reporter-first _)))

(define (tree message)
  "A reporter that prints the complete suite tree (like the @code{tree}
CLI command) when a top-level suite finishes loading."
  (case (assoc-ref message 'type)
    ((load/end)
     (let ((suite-node (assoc-ref message 'suite-node)))
       (format (get-port message) "\n~a"
               (suite-forest->tree-string (list suite-node)))))
    (else #f)))

(define (loaded-summary message)
  "A reporter that prints the number of loaded suites and tests
when a top-level suite finishes loading."
  (case (assoc-ref message 'type)
    ((load/end)
     (let* ((suite-node (assoc-ref message 'suite-node))
            (counts (count-suites-and-tests suite-node))
            (suites (assoc-ref counts 'suites))
            (tests (assoc-ref counts 'tests))
            (modules (assoc-ref counts 'module-suites))
            (empty (assoc-ref counts 'empty-suites)))
       (format (get-port message)
               "Loaded ~a test~p and ~a suite~p (~a module~p, ~a empty).\n"
               tests tests suites suites modules modules empty)))
    (else #f)))

(define (run-summary message)
  "A reporter that prints a summary line after all tests have been executed."
  (case (assoc-ref message 'type)
    ((run/end)
     (let ((summary (state:get-run-summary (assoc-ref message 'suitbl/state))))
       (if summary
           (let ((tests (assoc-ref summary 'tests))
                 (assertions (assoc-ref summary 'assertions))
                 (failures (assoc-ref summary 'failures))
                 (errors (assoc-ref summary 'errors)))
             (format (get-port message)
                     "Ran ~a assertion~p in ~a test~p: ~a failure~p, ~a error~p.\n"
                     assertions assertions
                     tests tests
                     failures failures
                     errors errors))
           (format (get-port message)
                   "No test results available.\n"))))
    (else #f)))

(define base
  (chain (list verbose
               ;; hierarchy
               tree
               loaded-summary
               run-summary)
    (reporter-every _)
    (list _ unhandled)
    (reporter-first _)))

(define (dots message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type
    ((run/test-start)
     (format (get-port message) "("))
    ((run/test-end)
     (format (get-port message) ")"))

    ((run/assertion-pass)
     (format (get-port message) "."))
    ((run/assertion-fail)
     (format (get-port message) "F"))
    ((run/assertion-error)
     (format (get-port message) "E"))

    (else #f)))

(define dots-with-hierarchy
  (chain (list dots hierarchy)
    (reporter-every _)
    (list _ unhandled)
    (reporter-first _)))

(define (junit message)
  "A test reporter that emits JUnit XML to the port specified via
@code{reporting/port} in the message after all tests have finished
running.  Silent for all other message types."
  (case (assoc-ref message 'type)
    ((run/end)
     (let* ((state (assoc-ref message 'suitbl/state))
            (forest (state:get-suite-forest-with-summary state))
            (xml (forest->junit-xml forest)))
       (format (get-port message) "~a\n" xml)))
    (else #f)))
