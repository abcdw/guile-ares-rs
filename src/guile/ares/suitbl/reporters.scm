;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl reporters)
  #:use-module ((ares suitbl core) #:select (test? suite?))
  #:use-module ((ares guile exceptions) #:select (exception->string))
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((srfi srfi-197) #:select (chain))

  #:use-module ((ice-9 exceptions) #:select (make-exception-with-message))
  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((ice-9 match) #:select (match))
  #:export (test-reporter-output-port*
            test-reporter-silent
            test-reporter-logging
            test-reporter-unhandled
            test-reporter-base
            test-reporter-dots
            test-reporter-dots-with-hierarchy
            test-reporter-minimal
            test-reporters-use-all
            test-reporters-use-first))



;;;
;;; Test Reporters
;;;

#|

Test reporters are simple functions which accept a message in format
of Association List (alist) and produce an output to
test-reporter-output-port*.

(test-reporter
 `((type . reporter/test-loaded)
   (suite-path . ("suite1" "nested-suite"))
   (description . "basic arithmetics")))


Test reporters can be comined with test-reporters-use-all or
test-reporters-use-first to compliment each other or override.

A final test reporter can be attached to test runner.

|#

(define test-reporter-output-port* (make-parameter (current-output-port)))

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

(define (test-reporter-silent message)
  "Do nothing, return @code{#t}."
  #t)

(define (test-reporter-logging message)
  "Just log the @code{message}."
  (format (test-reporter-output-port*) "message: ~y" message))

(define (test-reporter-unhandled message)
  "A simple test reporter, which prints incomming message.  It can be
combined with another reporter using @code{test-reporters-use-first}
to catch unhandled messages."
  (format (test-reporter-output-port*)
          "\nmessage is not handled:\n~y\n" message))

(define (string-repeat s n)
  "Returns string S repeated N times."
  (fold
   (lambda (_ str)
     (string-append str s))
   ""
   (iota n)))

(define (tests->pretty-string l)
  (map
   (lambda (i)
     (cond
      ((test? i) (string-append "test: " (assoc-ref i 'test/description)))
      ((suite? i)
       (string-append "suite: " (assoc-ref i 'suite/description)))
      ((list? i) (tests->pretty-string i))
      (else i)))
   l))

(define (test-reporter-hierarchy message)
  (case (assoc-ref message 'type)
    ((reporter/test-loaded)
     (format (test-reporter-output-port*) "~a"
             (string-repeat "|" (length (assoc-ref message 'suite-path))))
     (format (test-reporter-output-port*) " + test ~a\n"
             (assoc-ref message 'description)))
    ((reporter/suite-enter)
     (format (test-reporter-output-port*) "~a"
             (string-append
              (string-repeat "|" (length (assoc-ref message 'suite-path)))
              "┌"))
     (format (test-reporter-output-port*) "> ~a\n"
             (assoc-ref message 'description)))
    ((reporter/suite-leave)
     (format (test-reporter-output-port*) "~a"
             (string-append
              (string-repeat "|" (length (assoc-ref message 'suite-path)))
              "└"))
     (format (test-reporter-output-port*) "> ~a\n"
             (assoc-ref message 'description)))

    ((reporter/print-suite)
     (format (test-reporter-output-port*) "~y"
             (tests->pretty-string (assoc-ref message 'suite))))
    (else #f)))

(define flaky-throw
  (let ((throw? #f))
    (lambda ()
      (if throw?
          (begin (set! throw? #f)
                 (raise-exception
                  (make-exception-with-message
                   "flaky exception")))
          (begin (set! throw? #t) 'ho)))))

;; (is (eq? 'hi (flaky-throw)))

;; (is (eq? 'hey 'ho))

;; (is 78)

(define (safify-thunk thunk)
  (lambda ()
    (with-exception-handler
     (lambda (ex)
       `(exception . ,ex))
     (lambda ()
       `(value . ,(thunk)))
     #:unwind? #t)))

(define (actual message)
    (let* ((assert-body (assoc-ref message 'assert/body))
           (args-thunk (assoc-ref message 'assert/args-thunk))
           (safe-args-thunk (safify-thunk args-thunk)))
      ;; TODO: [Andrew Tropin, 2025-05-28] Ensure arguments-thunk
      ;; exception handled.
      (if (and (list? assert-body) (= 3 (length assert-body)))
          (match (safe-args-thunk)
            ((value . (first second))
             (format #f "~a and ~a are not ~a" first second (car assert-body)))
            ((exception . ex)
             (format #f "Evaluation of arguments thunk failed with:\n~a" ex)))
          (assoc-ref message 'assertion/result))))

(define (test-reporter-verbose message)
  (case (assoc-ref message 'type)
    ((reporter/test-start)
     (format (test-reporter-output-port*) "\n┌Test ~a\n"
             (assoc-ref message 'description)))
    ((reporter/test-end)
     (format (test-reporter-output-port*) "└Test ~a\n"
             (assoc-ref message 'description)))

    ((reporter/assertion-pass)
     (format (test-reporter-output-port*) "~y✓\n"
             (assoc-ref message 'assert/body)))

    ((reporter/assertion-fail)
     (format (test-reporter-output-port*) "~y✗ ~a\n"
             (assoc-ref message 'assert/body) (actual message)))

    ((reporter/assertion-error)
     (format (test-reporter-output-port*) "~y✗ produced error:\n ~s\n"
             (assoc-ref message 'assert/body)
             (exception->string
              (assoc-ref message 'assertion/error))))

    (else #f)))

(define (test-reporter-loading-minimal message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type
    ((reporter/test-loaded)
     (format (test-reporter-output-port*) "-> ~a\n"
             (assoc-ref message 'description)))

    ;; ((reporter/suite-enter)
    ;;  (format (test-reporter-output-port*) "["))
    ;; ((reporter/suite-leave)
    ;;  (format (test-reporter-output-port*) "]"))
    (else #f)))

(define (test-reporter-execution-minimal message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type

    ((reporter/test-start)
     (format (test-reporter-output-port*) "=> [~a]\n"
             (assoc-ref message 'description)))
    ((reporter/test-end)
     (format (test-reporter-output-port*) "\n"
             (assoc-ref message 'description)))

    ((reporter/assertion-pass)
     (format (test-reporter-output-port*) "✓"))

    ((reporter/assertion-fail)
     (format (test-reporter-output-port*) "\n ~y✗ ~a\n"
             (assoc-ref message 'assert/body) (actual message)))

    ((reporter/assertion-error)
     (format (test-reporter-output-port*) "\n ~y✗ produced error:\n ~s\n"
             (assoc-ref message 'assert/body)
             (exception->string
              (assoc-ref message 'assertion/error))))

    (else #f)))

(define test-reporter-minimal
  (chain (list
          test-reporter-execution-minimal
          test-reporter-loading-minimal)
    (test-reporters-use-all _)
    (list _ test-reporter-unhandled)
    (test-reporters-use-first _)))

(define test-reporter-base
  (chain (list test-reporter-verbose test-reporter-hierarchy)
    (test-reporters-use-all _)
    (list _ test-reporter-unhandled)
    (test-reporters-use-first _)))

(define (test-reporter-dots message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type
    ((reporter/suite-start)
     (format (test-reporter-output-port*) "["))
    ((reporter/suite-end)
     (format (test-reporter-output-port*) "]"))

    ((reporter/test-start)
     (format (test-reporter-output-port*) "("))
    ((reporter/test-end)
     (format (test-reporter-output-port*) ")"))
    ((reporter/test-skip)
     (format (test-reporter-output-port*) "(S)"))

    ((reporter/assertion-pass)
     (format (test-reporter-output-port*) "."))
    ((reporter/assertion-fail)
     (format (test-reporter-output-port*) "F"))
    ((reporter/assertion-error)
     (format (test-reporter-output-port*) "E"))

    (else #f)))

(define test-reporter-dots-with-hierarchy
  (chain (list test-reporter-dots test-reporter-hierarchy)
    (test-reporters-use-all _)
    (list _ test-reporter-unhandled)
    (test-reporters-use-first _)))
