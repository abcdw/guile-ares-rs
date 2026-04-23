;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl reporters)
  #:use-module ((ares suitbl state) #:prefix state:)
  #:use-module ((ares suitbl running) #:prefix running:)
  #:use-module ((ares suitbl reporting) #:prefix reporting:)
  #:use-module ((srfi srfi-1) #:select (alist-delete fold))
  #:use-module ((srfi srfi-197) #:select (chain chain-and))

  #:use-module ((ice-9 format) #:select (format))

  #:export (reporter-every
            reporter-first
            silent
            logging
            make-spying
            make-ignore-reporter
            unhandled
            load-ignore-messages
            verbose-all
            verbose-only-failed
            base
            base-all
            minimal
            compact
            junit
            load-tree
            load-summary
            run-dots
            run-dots-extended
            run-plan-compact
            run-summary
            zero-assertion-warning))


;;;
;;; Reporter Combinators
;;;

(define (reporter-every reporters)
  "Create a reporter, which combines all reporters.  Returns @code{#t}
if any reporter returns a truthy value."
  (lambda (message)
    (fold (lambda (r acc)
            (if (r message) #t acc))
          #f
          reporters)))

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
   (test . ((test/description . "basic arithmetics")))))


Test reporters can be combined with reporter-every or
reporter-first to complement each other or override.

A final test reporter can be attached to test runner.

|#

(define (get-port message)
  (or (assoc-ref message 'reporting/port) (current-output-port)))

(define (message-test-description message)
  (let ((test (assoc-ref message 'test)))
    (and (list? test)
         (assoc-ref test 'test/description))))

(define (silent message)
  "Do nothing, return @code{#t}."
  #t)

(define (logging message)
  "Just log the @code{message}."
  (format (get-port message) "message: ~y" message))

(define (default-spying-transform message)
  (chain message
    (alist-delete 'suitbl/state _)
    (format #f "~y\n" _)))

(define* (make-spying types
                      #:key
                      (predicate (lambda (_) #t))
                      (transform default-spying-transform))
  "Return a reporter that writes transformed messages whose type is in
TYPES.  Useful for debugging reporter traffic.  When PREDICATE is
provided, print only messages for which it returns a truthy value.
TRANSFORM should accept MESSAGE and return a string to write to the
reporting port."
  (lambda (message)
    (and (memq (assoc-ref message 'type) types)
         (predicate message)
         (format (get-port message) "~a" (transform message)))))

(define (unhandled message)
  "A simple test reporter, which prints incomming message.  It can be
combined with another reporter using @code{reporter-first}
to catch unhandled messages."
  (format (get-port message)
          "\nmessage is not handled: ~y\n"
          (assoc-ref message 'type))
  ;; (force-output (current-error-port))
  )

(define (make-ignore-reporter types)
  "Return a reporter that silently handles messages whose type is in
TYPES."
  (lambda (message)
    (and (memq (assoc-ref message 'type) types) #t)))

(define %load-ignore-messages
  (make-ignore-reporter '(load/test load/suite-enter load/suite-leave)))

(define (load-ignore-messages message)
  "Silently handle load-phase messages to avoid noisy unhandled output."
  (%load-ignore-messages message))

(define (%write-verbose-test-run port test-run)
  (chain-and test-run
    (reporting:format-test-run-verbose _)
    (format port "~a" _)))

(define (write-verbose-test-run message)
  (chain-and message
    (assoc-ref _ 'test-run)
    (%write-verbose-test-run (get-port message) _)))

(define (failed-test-run? test-run)
  (memq (assoc-ref test-run 'test-run/outcome) '(fail error)))

(define (verbose-all message)
  (case (assoc-ref message 'type)
    ((run/test-end)
     (write-verbose-test-run message))

    (else #f)))

(define (verbose-only-failed message)
  (case (assoc-ref message 'type)
    ((run/test-end)
     (and (chain-and message
            (assoc-ref _ 'test-run)
            (failed-test-run? _))
          (write-verbose-test-run message))
     #t)

    (else #f)))

(define (load-minimal message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type
    ((load/test)
     (format (get-port message) "-> ~a\n"
             (message-test-description message)))

    (else #f)))

(define (run-minimal message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type

    ((run/test-start)
     (format (get-port message) "--- [~a] ---\n"
             (message-test-description message)))
    ((run/test-end)
     (format (get-port message) "\n"))

    ((run/assertion-end)
     (let ((formatted
            (chain message
              (assoc-ref _ 'assertion-run)
              (reporting:format-assertion-minimal _))))
       (and formatted
            (format (get-port message) "~a" formatted))))

    (else #f)))

(define (zero-assertion-warning message)
  "Warn at run end about tests that executed zero assertions."
  (define port (get-port message))
  (define (zero-assertion-test-descriptions state)
    (reverse
     (fold (lambda (entry result)
             (let* ((extended-outcome (assoc-ref entry 'test-run/extended-outcome)))
               (if (eq? 'zero-assertions extended-outcome)
                   (cons (reporting:format-test-twoline (assoc-ref entry 'test))
                         result)
                   result)))
           '()
           (or (state:get-run-history state) '()))))

  (case (assoc-ref message 'type)
    ((run/end)
     (let* ((descriptions
             (zero-assertion-test-descriptions
              (assoc-ref message 'suitbl/state)))
            (count (length descriptions)))
       (unless (zero? count)
         (format port
                 "Warning: ~a test~p executed zero assertions:\n"
                 count count)
         (for-each (lambda (description)
                     (format port "- ~a\n" description))
                   descriptions))
       #t))
    (else #f)))

(define (load-tree message)
  "A reporter that prints the complete suite tree (like the @code{tree}
CLI command) when a top-level suite finishes loading."
  (case (assoc-ref message 'type)
    ((load/end)
     (let ((suite-node (assoc-ref message 'suite-node)))
       (format (get-port message) "\n~a"
               (reporting:suite-forest->tree-string (list suite-node)))))
    (else #f)))

(define (load-summary message)
  "A reporter that prints the number of loaded suites and tests
when a top-level suite finishes loading."
  (case (assoc-ref message 'type)
    ((load/end)
     (let* ((suite-node (assoc-ref message 'suite-node))
            (counts (reporting:count-suites-and-tests suite-node))
            (suites (assoc-ref counts 'suites))
            (tests (assoc-ref counts 'tests))
            (modules (assoc-ref counts 'module-suites))
            (empty (assoc-ref counts 'empty-suites)))
       (format (get-port message)
               "Loaded ~a test~p and ~a suite~p (~a module~p, ~a empty).\n"
               tests tests suites suites modules modules empty)))
    (else #f)))

(define (run-plan-compact message)
  "A reporter that prints a single compact line at the start of a test
run, showing how many tests were scheduled out of the loaded total.
Expects a @code{run-plan} alist on @code{run/start} messages with
@code{plan/scheduled-count} and @code{plan/loaded-count} keys."
  (case (assoc-ref message 'type)
    ((run/start)
     (let* ((plan (or (assoc-ref message 'run-plan) '()))
            (scheduled (or (assoc-ref plan 'plan/scheduled-count) 0))
            (loaded (or (assoc-ref plan 'plan/loaded-count) scheduled)))
       (format (get-port message)
               "Running ~a of ~a loaded test~p...\n"
               scheduled loaded loaded)))
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

(define (run-failed-test-runs message)
  "Print verbose reports for failed and erroring test runs at run end."
  (case (assoc-ref message 'type)
    ((run/end)
     (let ((run-history
            (chain-and message
              (assoc-ref _ 'suitbl/state)
              (state:get-run-history _))))
       (let loop ((history run-history))
         (unless (null? history)
           (let ((test-run (car history)))
             (when (failed-test-run? test-run)
               (%write-verbose-test-run (get-port message) test-run))
             (loop (cdr history)))))
       #t))
    (else #f)))

(define %run-dots-line-width 50)

(define (make-run-dots outcome-key outcome->char legend)
  "Return a dots reporter using OUTCOME-KEY to look up the test outcome,
OUTCOME->CHAR to map it to a display character, and LEGEND to explain
those characters at @code{run/start}.  Shows at most 50 tests per line
with a right-aligned counter when @code{run-progress} is present in the
message."
  (lambda (message)
    (define (print-outcome port outcome)
      (format port "~a" (outcome->char outcome)))

    (define (print-counter port current total)
      (when (and current total)
        (let* ((line-pos (modulo (1- current) %run-dots-line-width))
               (end-of-line?
                (or (= current total)
                    (= (1+ line-pos) %run-dots-line-width))))
          (when end-of-line?
            (let* ((dots-on-line (1+ line-pos))
                   (padding (- %run-dots-line-width dots-on-line))
                   (total-width (string-length (number->string total))))
              (format port "~a  [~vd/~a]\n"
                      (make-string padding #\space)
                      total-width current total))))))

    (case (assoc-ref message 'type)
      ((run/start)
       (format (get-port message) "Legend: ~a\n" legend)
       #t)
      ((run/test-start)    #t)
      ((run/assertion-end) #t)
      ((run/test-end)
       (let* ((port (get-port message))
              (outcome (chain-and message
                         (assoc-ref _ 'test-run)
                         (assoc-ref _ outcome-key)))
              (progress (assoc-ref message 'run-progress))
              (current (and progress (assoc-ref progress 'progress/current)))
              (total (and progress (assoc-ref progress 'progress/total))))
         (print-outcome port outcome)
         (print-counter port current total)
         (force-output port)
         #t))
      ((run/end) #t)
      (else #f))))

(define (make-newline-reporter types)
  (lambda (message)
    (define msg-type (assoc-ref message 'type))
    (define port (get-port message))
    (when (member msg-type types)
      (newline port))
    #f))

(define run-dots
  (make-run-dots
   'test-run/outcome
   (lambda (outcome)
     (case outcome
       ((pass)  ".")
       ((fail)  "F")
       ((error) "E")
       (else    "?")))
   ".=pass, F=fail, E=error"))

(define run-dots-extended
  (make-run-dots
   'test-run/extended-outcome
   (lambda (outcome)
     (case outcome
       ((pass)         ".")
       ((fail)         "F")
       ((error)        "E")
       ((zero-assertions) "Z")
       ((aborted)      "A")
       (else           "?")))
   ".=pass, Z=zero assertions, F=fail, E=error, A=aborted"))

(define compact
  (chain (list
          run-plan-compact

          run-dots-extended
          run-failed-test-runs

          (make-newline-reporter '(run/end))
          zero-assertion-warning
          (make-newline-reporter '(run/end))
          run-summary

          load-summary
          load-ignore-messages)
    (reporter-every _)
    (list _ unhandled)
    (reporter-first _)))

(define (make-base-reporter verbose-reporter)
  (chain (list verbose-reporter
               (make-ignore-reporter '(run/test-start run/assertion-end))

               (make-newline-reporter '(run/end))
               zero-assertion-warning
               (make-newline-reporter '(run/end))

               load-ignore-messages
               load-tree
               load-summary
               run-plan-compact
               run-summary)
    (reporter-every _)
    (list _ unhandled)
    (reporter-first _)))

(define base
  (make-base-reporter verbose-only-failed))

(define base-all
  (make-base-reporter verbose-all))

(define minimal
  (chain (list
          run-minimal
          zero-assertion-warning
          run-summary
          load-ignore-messages
          load-minimal)
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
            (xml (reporting:forest->junit-xml forest)))
       (format (get-port message) "~a\n" xml)))
    (else #f)))
