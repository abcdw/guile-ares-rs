;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl reporters)
  #:use-module ((ares suitbl state) #:prefix state:)
  #:use-module ((ares suitbl running) #:prefix running:)
  #:use-module ((ares suitbl reporting)
                #:select (format-location
                          actual pre-evaled-expression
                          tree-node-children
                          tree-node-description
                          suite-forest->tree-string
                          count-suites-and-tests
                          forest->junit-xml))
  #:use-module ((ares guile exceptions) #:select (exception->string))
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((srfi srfi-197) #:select (chain chain-and))

  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((ice-9 match) #:select (match))

  #:export (reporter-every
            reporter-first
            silent
            logging
            unhandled
            load-ignore-messages
            base
            minimal
            spying
            compact
            junit
            load-tree
            load-summary
            run-dots
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

(define (unhandled message)
  "A simple test reporter, which prints incomming message.  It can be
combined with another reporter using @code{reporter-first}
to catch unhandled messages."
  (format (get-port message)
          "\nmessage is not handled: ~y\n"
          (assoc-ref message 'type))
  ;; (force-output (current-error-port))
  )

(define (load-ignore-messages message)
  "Silently handle load-phase messages to avoid noisy unhandled output."
  (case (assoc-ref message 'type)
    ((load/test load/suite-enter load/suite-leave) #t)
    (else #f)))

(define (verbose message)
  (case (assoc-ref message 'type)
    ((run/test-start)
     (format (get-port message) "\n┌Test ~a\n"
             (message-test-description message)))
    ((run/test-end)
     (format (get-port message) "└Test ~a\n"
             (message-test-description message)))

    ((run/assertion-end)
     (let ((outcome (chain-and message
                      (assoc-ref _ 'assertion-run)
                      (assoc-ref _ 'assertion-run/outcome)))
           (run-result (chain-and message
                         (assoc-ref _ 'assertion-run)
                         (assoc-ref _ 'assertion-run/result)))
           (assert-body (chain-and message
                          (assoc-ref _ 'assertion)
                          (assoc-ref _ 'assert/body)))
           (assert-location (chain-and message
                              (assoc-ref _ 'assertion)
                              (assoc-ref _ 'assert/location))))
       (case outcome
         ((pass)
          (format (get-port message) "~y✓\n" assert-body))
         ((fail)
          (format (get-port message) "~a\n~y✗ ~a\n"
                  (format-location assert-location)
                  assert-body
                  (actual message)))
         ((error)
          (format (get-port message) "~a\n~y✗ produced error:\n ~s\n"
                  (format-location assert-location)
                  assert-body
                  (exception->string
                   (and (running:raised? run-result)
                        (running:raised-exception run-result)))))
         (else #f))))

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
     (let ((outcome (chain-and message
                      (assoc-ref _ 'assertion-run)
                      (assoc-ref _ 'assertion-run/outcome)))
           (run-result (chain-and message
                         (assoc-ref _ 'assertion-run)
                         (assoc-ref _ 'assertion-run/result)))
           (assert-body (chain-and message
                          (assoc-ref _ 'assertion)
                          (assoc-ref _ 'assert/body)))
           (assert-location (chain-and message
                              (assoc-ref _ 'assertion)
                              (assoc-ref _ 'assert/location))))
       (case outcome
         ((pass)
          (format (get-port message) "✓"))
         ((fail)
          (format (get-port message) "~a\n~y✗ ~a\n"
                  (format-location assert-location)
                  assert-body
                  (actual message)))
         ((error)
          (format (get-port message) "~a\n~y✗ produced error:\n ~s\n"
                  (format-location assert-location)
                  assert-body
                  (exception->string
                   (and (running:raised? run-result)
                        (running:raised-exception run-result)))))
         (else #f))))

    (else #f)))

(define (zero-assertion-warning message)
  "Warn when a test finishes without executing any assertions."
  (case (assoc-ref message 'type)
    ((run/test-end)
     (let* ((summary (chain-and message
                       (assoc-ref _ 'test-run)
                       (assoc-ref _ 'test-run/summary)))
            (assertions (and summary
                             (assoc-ref summary 'assertions))))
       (if (and assertions (zero? assertions))
           (begin
             (format (get-port message)
                     "\nwarning: Test `~a' executed zero assertions.\n"
                     (message-test-description message))
             #t)
           #f)))
    (else #f)))

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

(define (execution-spying message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type

    ((run/test-start)
     (format (get-port message) "--- [~a] ---\n"
             (message-test-description message)))
    ((run/test-end)
     (format (get-port message) "\n"))

    ((run/assertion-end)
     (let ((outcome (chain-and message
                      (assoc-ref _ 'assertion-run)
                      (assoc-ref _ 'assertion-run/outcome)))
           (run-result (chain-and message
                         (assoc-ref _ 'assertion-run)
                         (assoc-ref _ 'assertion-run/result)))
           (assert-body (chain-and message
                          (assoc-ref _ 'assertion)
                          (assoc-ref _ 'assert/body)))
           (assert-location (chain-and message
                              (assoc-ref _ 'assertion)
                              (assoc-ref _ 'assert/location))))
       (case outcome
         ((pass fail)
          (format (get-port message) "~y~y => ~y"
                  assert-body
                  (pre-evaled-expression message)
                  (and (running:returned? run-result)
                       (running:returned-value run-result))))
         ((error)
          (format (get-port message) "\n ~a\n ~y✗ produced error:\n ~s\n"
                  (format-location assert-location)
                  assert-body
                  (exception->string
                   (and (running:raised? run-result)
                        (running:raised-exception run-result)))))
         (else #f))))

    (else #f)))

(define spying
  (chain (list
          execution-spying
          zero-assertion-warning
          load-minimal)
    (reporter-every _)
    (list _ unhandled)
    (reporter-first _)))

(define (load-tree message)
  "A reporter that prints the complete suite tree (like the @code{tree}
CLI command) when a top-level suite finishes loading."
  (case (assoc-ref message 'type)
    ((load/end)
     (let ((suite-node (assoc-ref message 'suite-node)))
       (format (get-port message) "\n~a"
               (suite-forest->tree-string (list suite-node)))))
    (else #f)))

(define (load-summary message)
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

(define %run-dots-line-width 50)

(define (run-dots message)
  "A compact test reporter that prints a single character per test:
@code{.} for pass, @code{F} for fail, @code{E} for error.  Shows at
most 50 tests per line with a right-aligned counter.  When a
@code{run-progress} alist is present in the message (with keys
@code{progress/current} and @code{progress/total}), prints a
@code{N/TOTAL} suffix at every 50th test and at the end of the run."
  (case (assoc-ref message 'type)
    ((run/test-start)
     #t)

    ((run/assertion-end)
     #t)

    ((run/test-end)
     (let* ((port (get-port message))
            (outcome (chain-and message
                       (assoc-ref _ 'test-run)
                       (assoc-ref _ 'test-run/outcome)))
            (progress (assoc-ref message 'run-progress))
            (current (and progress (assoc-ref progress 'progress/current)))
            (total (and progress (assoc-ref progress 'progress/total))))

       (format port "~a"
               (case outcome
                 ((pass)  ".")
                 ((fail)  "F")
                 ((error) "E")
                 (else    "?")))

       (when (and current total)
         (let* ((line-pos (modulo (1- current) %run-dots-line-width))
                (end-of-line?
                 (or (= current total)
                     (= (1+ line-pos) %run-dots-line-width))))

           (when end-of-line?
             (let* ((dots-on-line (1+ line-pos))
                    (padding (- %run-dots-line-width dots-on-line))
                    (total-width (string-length
                                  (number->string total))))
               (format port "~a  ~vd/~a\n"
                       (make-string padding #\space)
                       total-width current
                       total)))))
       (force-output port)
       #t))

    ((run/end)
     #t)

    (else #f)))

(define compact
  (chain (list
          run-dots

          run-plan-compact
          run-summary

          load-summary
          load-ignore-messages)
    (reporter-every _)
    (list _ unhandled)
    (reporter-first _)))

(define base
  (chain (list verbose
               zero-assertion-warning
               load-ignore-messages
               load-tree
               load-summary
               run-plan-compact
               run-summary)
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
