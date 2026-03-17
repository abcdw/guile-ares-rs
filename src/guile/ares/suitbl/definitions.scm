;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl definitions)
  #:export (test-runner*

            is
            test test?
            test-thunk
            suite suite?
            suite-thunk suite-thunk?

            define-suite))



;;;
;;; Tasks before SRFI finalization
;;;

;; TODO: [Andrew Tropin, 2025-08-06] Make all the tests to be disabled
;; by default. Useful for production code to make no test code leaks
;; into it.

;; TODO: [Andrew Tropin, 2025-08-27] Write tests to check that test
;; runner gets all the necessary information from test definitions.

;; IDEA: [Andrew Tropin, 2025-09-18] Change API of define-suite to
;; mimic usual define, so the name of the suite is wrapped with
;; parentesis and can be immediately called.

;; IDEA: [Andrew Tropin, 2025-09-18] Add support for docstrings for
;; define-suite

;; TODO: [Andrew Tropin, 2026-03-17] Decide default recommended
;; semantics for evaluation of is, test, suite.


;;;
;;; Test Definitions API
;;;

(define (missing-test-runner _)
  (format (current-error-port) "\
The test-runner* is not set. Probably you imported test defining API
directly instead of using a downstream testing library. That's
probably not what you want, unless you are a developer of a testing
library and enjoy seeing this message. Please, use suitbl or other
library, which sets an approriate test runner for you."))

(define test-runner* (make-parameter missing-test-runner))

(define (test? x)
  (and (list? x)
       (assoc-ref x 'test/body-thunk)
       (assoc-ref x 'test/description)))

(define (suite? x)
    (and (list? x)
         (assoc-ref x 'suite/body-thunk)
         (assoc-ref x 'suite/description)))

(define (suite-thunk? x)
  (and (procedure? x)
       (procedure-property x 'suitbl-suite-thunk?)))

(define-syntax is
  (lambda (stx)
    "A flexible assert macro.  The behavior can be customized by test runner."
    (syntax-case stx ()
      ((_ (pred args ...))
       #'((test-runner*)
          `((type . runner/run-assert)
            (assert . ((assert/body-thunk . ,(lambda () (pred args ...)))
                       (assert/args-thunk . ,(lambda () (list args ...)))
                       (assert/body . (pred args ...)))))))
      ((_ form)
       #'((test-runner*)
          `((type . runner/run-assert)
            (assert . ((assert/body-thunk . ,(lambda () form))
                       (assert/body . form)))))))))

(define (alist-merge l1 l2)
  (append l1 l2))

(define-syntax test-thunk
  (lambda (stx)
    (syntax-case stx (metadata)
      ((_ test-description (quote metadata) metadata-value expression expressions ...)
       #'(let ((test-entity
                `((test/body-thunk . ,(lambda () expression expressions ...))
                  (test/body . (expression expressions ...))
                  (test/description . ,test-description)
                  (test/metadata . ,metadata-value))))
           (lambda ()
             ((test-runner*)
              `((type . runner/load-test)
                (test . ,test-entity))))))

      ((_ test-description expression expressions ...)
       #'(test-thunk test-description 'metadata '() expression expressions ...)))))

(define-syntax test
  (lambda (stx)
    "Test represent a logical unit of testing, usually includes zero or
more @code{is} asserts."
    (syntax-case stx ()
      ((_ test-description arguments ...)
       #'((test-thunk test-description arguments ...))))))

(define-syntax suite-thunk
  (lambda (stx)
    (syntax-case stx (metadata)
      ((_ suite-description (quote metadata) metadata-value
          expression expressions ...)
       #'(let* ((suite-entity
                 `((suite/body-thunk . ,(lambda () expression expressions ...))
                   (suite/description . ,suite-description)
                   (suite/metadata . ,metadata-value)))

                (%suite-thunk
                 ;; Wrapping into identity to prevent setting procedure-name
                 (identity
                  (lambda ()
                    ((test-runner*)
                     `((type . runner/load-suite)
                       (suite . ,suite-entity)))))))

           (set-procedure-properties!
            %suite-thunk
            `((documentation . ,suite-description)
              (suite . ,suite-entity)
              (suitbl-suite-thunk? . #t)))
           %suite-thunk))

      ((_ suite-description expression expressions ...)
       #'(suite-thunk
          suite-description 'metadata '() expression expressions ...)))))

(define-syntax suite
  (lambda (stx)
    "Test suite is a grouping unit, it allows to combine tests and other
test suites."
    (syntax-case stx ()
      ((_ suite-description arguments ...)
       #'((suite-thunk suite-description arguments ...))))))

(define-syntax define-suite
  (lambda (stx)
    "Equivalent of (define-public NAME (suite-thunk ...))."
    (syntax-case stx ()
      ((_ suite-name expression ...)
       #'(define-public suite-name
           (suite-thunk (symbol->string 'suite-name) expression ...))))))
