;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl core)
  #:export (test-runner*

            suite test is
            suite-thunk test-thunk

            ;; TODO: [Andrew Tropin, 2025-05-15] Remove it?, because it
            ;; introduces ambiguity and doesn't have a private
            ;; counterpart
            define-suite))



;;;
;;; Tasks before SRFI finalization
;;;

;; TODO: [Andrew Tropin, 2025-08-06] Make all the tests to be disabled
;; by default. Useful for production code to make no test code leaks
;; into it.



;;;
;;; Core Test Definition API
;;;

(define (missing-test-runner _)
  (format (current-error-port) "\
The test-runner* is not set. Probably you imported test defining API
directly instead of using a wrapping testing library. That's probably
not what you want, unless you are a developer of a testing
library. Please, use suitbl or other library, which set an approriate
test runner or set it manually."))

(define test-runner* (make-parameter missing-test-runner))

(define (test? x)
  (and (list? x)
       (assoc-ref x 'test/body-thunk)))

(define (suite? x)
    (and (list? x)
         (assoc-ref x 'suite/body-thunk)))

(define (suite-thunk? x)
  (and (procedure? x)
       (procedure-property x 'suitbl-suite-thunk?)))

;; We use syntax-rules because it save patterns into transformer's
;; metadata, which allows to generate "signature" of the macro.

(define-syntax is
  (syntax-rules ()
    "A flexible assert macro.  The behavior can be customized by test runner."
    ((_ (pred args ...))
     ((test-runner*)
      `((type . run-assert)
        (assert . ((assert/body-thunk . ,(lambda () (pred args ...)))
                   (assert/args-thunk . ,(lambda () (list args ...)))
                   (assert/body . (pred args ...)))))))
    ((_ form)
     ((test-runner*)
      `((type . run-assert)
        (assert . ((assert/body-thunk . ,(lambda () form))
                   (assert/body . form))))))))

(define (alist-merge l1 l2)
  (append l1 l2))

(define-syntax test-thunk
  (syntax-rules ()
    ((test-thunk test-description 'metadata metadata expression expressions ...)
     (let ((test `((test/body-thunk . ,(lambda () expression expressions ...))
                   (test/body . (expression expressions ...))
                   (test/description . ,test-description)
                   (test/metadata . ,metadata))))
       (lambda ()
         ((test-runner*)
          `((type . load-test)
            (test . ,test))))))

    ((test-thunk test-description expression expressions ...)
     (test-thunk test-description 'metadata '() expression expressions ...))))

(define-syntax test
  (syntax-rules ()
    "Test represent a logical unit of testing, usually includes zero or
more @code{is} asserts."
    ((test test-description arguments ...)
     ((test-thunk test-description arguments ...)))))

(define-syntax suite-thunk
  (syntax-rules ()
    ((_ suite-description 'metadata metadata expression expressions ...)
     (let* ((suite
             `((suite/body-thunk . ,(lambda () expression expressions ...))
               (suite/description . ,suite-description)
               (suite/metadata . ,metadata)))

            (suite-thunk
                ;; Wrapping into identity to prevent setting procedure-name
                (identity
                 (lambda ()
                   ((test-runner*)
                    `((type . load-suite)
                      (suite . ,suite)))))))

       (set-procedure-properties!
        suite-thunk
        `((documentation . ,suite-description)
          (suite . ,suite)
          (suitbl-suite-thunk? . #t)))
       suite-thunk))

    ((suite-thunk suite-description expression expressions ...)
     (suite-thunk
         suite-description 'metadata '() expression expressions ...))))

(define-syntax suite
  (syntax-rules ()
    "Test suite is a grouping unit, it allows to combine tests and other
test suites."
    ((suite suite-description arguments ...)
     ((suite-thunk suite-description arguments ...)))))

(define-syntax define-suite
  (syntax-rules ()
    "Equivalent of (define-public NAME (suite-thunk ...))."
    ((_ suite-name expression ...)
     (define-public suite-name
       (suite-thunk (symbol->string 'suite-name) expression ...)))))
