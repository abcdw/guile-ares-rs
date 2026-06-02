;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl definitions)
  #:export (test-runner*
            set-test-runner!

            is
            test test?
            test-loader
            test-thunk
            suite suite?
            suite-loader suite-loader?
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

(define (set-test-runner! runner)
  "Set the current test runner to RUNNER and return the previous runner."
  (test-runner* runner))

(define (test? x)
  (and (list? x)
       (assoc-ref x 'test/body-procedure)
       (assoc-ref x 'test/description)))

(define (suite? x)
    (and (list? x)
         (assoc-ref x 'suite/body-thunk)
         (assoc-ref x 'suite/description)))

(define (suite-loader? x)
  (and (procedure? x)
       (or (procedure-property x 'suitbl-suite-loader?)
           (procedure-property x 'suitbl-suite-thunk?))))

(define (suite-thunk? x)
  (warn-deprecated-suite-thunk-predicate)
  (suite-loader? x))

(define (make-source-absolute source)
  "Make the filename in a syntax SOURCE alist absolute.  If the
filename is relative, it is resolved by searching in @code{%load-path}
at macro-expansion time."
  ;; Guile-specific implementation
  (if (not source)
      source
      (let ((filename (assoc-ref source 'filename)))
        (cond
         ((not filename) source)
         ((absolute-file-name? filename) source)
         (else
          (let ((found (%search-load-path filename)))
            (if found
                (assoc-set! (list-copy source)
                            'filename
                            (if (absolute-file-name? found)
                                found
                                (string-append (getcwd) "/" found)))
                source)))))))

(define-syntax is
  (lambda (stx)
    "A flexible assertion macro.  The behavior can be customized by test runner."
    (define (build-assertion stx fields)
      (with-syntax ((location (datum->syntax
                               stx
                               (make-source-absolute (syntax-source stx))))
                    ((assertion-field ...) fields))
        #'((test-runner*)
           `((type . runner/run-assert)
             (assertion . (assertion-field ...
                           (assertion/location . location)))))))

    (syntax-case stx ()
      ((_ (pred args ...) description)
       (build-assertion stx
                        #'((assertion/body-thunk
                            . ,(lambda () (pred args ...)))
                           (assertion/args-thunk
                            . ,(lambda () (list args ...)))
                           (assertion/body . (pred args ...))
                           (assertion/description . ,description))))
      ((_ form description)
       (build-assertion stx
                        #'((assertion/body-thunk . ,(lambda () form))
                           (assertion/body . form)
                           (assertion/description . ,description))))
      ((_ (pred args ...))
       (build-assertion stx
                        #'((assertion/body-thunk
                            . ,(lambda () (pred args ...)))
                           (assertion/args-thunk
                            . ,(lambda () (list args ...)))
                           (assertion/body . (pred args ...)))))
      ((_ form)
       (build-assertion stx
                        #'((assertion/body-thunk . ,(lambda () form))
                           (assertion/body . form)))))))

(define (alist-merge l1 l2)
  (append l1 l2))

(define (warn-deprecated-test-form location)
  (let ((port (current-warning-port)))
    (format port "warning: deprecated suitbl test form")
    (when (and location (assoc-ref location 'filename))
      (format port " at ~a" (assoc-ref location 'filename))
      (when (assoc-ref location 'line)
        (format port ":~a" (assoc-ref location 'line)))
      (when (assoc-ref location 'column)
        (format port ":~a" (assoc-ref location 'column))))
    (format port "\n")
    (format port "  `(test DESCRIPTION BODY ...)` syntax is deprecated.\n")
    (format port "  Use `(test (DESCRIPTION _) BODY ...)` or `(test (DESCRIPTION context) BODY ...)`.")
    (format port "\n")))

(define (warn-deprecated-test-thunk location)
  (let ((port (current-warning-port)))
    (format port "warning: deprecated suitbl test-thunk form")
    (when (and location (assoc-ref location 'filename))
      (format port " at ~a" (assoc-ref location 'filename))
      (when (assoc-ref location 'line)
        (format port ":~a" (assoc-ref location 'line)))
      (when (assoc-ref location 'column)
        (format port ":~a" (assoc-ref location 'column))))
    (format port "\n")
    (format port "  Use `(test-loader DESCRIPTION BODY ...)` instead.")
    (format port "\n")))

(define (warn-deprecated-suite-thunk location)
  (let ((port (current-warning-port)))
    (format port "warning: deprecated suitbl suite-thunk form")
    (when (and location (assoc-ref location 'filename))
      (format port " at ~a" (assoc-ref location 'filename))
      (when (assoc-ref location 'line)
        (format port ":~a" (assoc-ref location 'line)))
      (when (assoc-ref location 'column)
        (format port ":~a" (assoc-ref location 'column))))
    (format port "\n")
    (format port "  Use `(suite-loader DESCRIPTION BODY ...)` instead.")
    (format port "\n")))

(define (warn-deprecated-suite-thunk-predicate)
  (let ((port (current-warning-port)))
    (format port "warning: deprecated suitbl suite-thunk? predicate\n")
    (format port "  Use `suite-loader?` instead.")
    (format port "\n")))

(define-syntax test-loader
  (lambda (stx)
    (define (build-test-loader stx description metadata body-procedure body deprecated?)
      (with-syntax ((location (datum->syntax
                               stx
                               (make-source-absolute (syntax-source stx))))
                    (test-description description)
                    (metadata-value metadata)
                    (test-body-procedure body-procedure)
                    ((test-body ...) body)
                    (deprecated-test-form? deprecated?))
        #'(begin
            (when deprecated-test-form?
              (warn-deprecated-test-form 'location))
            (let ((test-entity
                   `((test/body-procedure . ,test-body-procedure)
                     (test/body . (test-body ...))
                     (test/description . ,test-description)
                     (test/metadata . ,metadata-value)
                     (test/location . location))))
              (lambda ()
                ((test-runner*)
                 `((type . runner/load-test)
                   (test . ,test-entity))))))))

    (syntax-case stx (metadata)
      ((_ (test-description context-name)
          (quote metadata) metadata-value expression expressions ...)
       (identifier? #'context-name)
       (build-test-loader stx
                          #'test-description
                          #'metadata-value
                          #'(lambda (context-name)
                              expression expressions ...)
                          #'(expression expressions ...)
                          #'#f))

      ((_ test-description (quote metadata) metadata-value expression expressions ...)
       (build-test-loader stx
                          #'test-description
                          #'metadata-value
                          #'(lambda (%suitbl-context)
                              expression expressions ...)
                          #'(expression expressions ...)
                          #'#t))

      ((_ test-head expression expressions ...)
       #'(test-loader test-head 'metadata '() expression expressions ...)))))

(define-syntax test-thunk
  (lambda (stx)
    (syntax-case stx ()
      ((_ arguments ...)
       (with-syntax ((location (datum->syntax
                                stx
                                (make-source-absolute (syntax-source stx)))))
         #'(begin
             (warn-deprecated-test-thunk 'location)
             (test-loader arguments ...)))))))

(define-syntax test
  (lambda (stx)
    "Test represent a logical unit of testing, usually includes zero or
more @code{is} asserts."
    (syntax-case stx ()
      ((_ test-description arguments ...)
       #'((test-loader test-description arguments ...))))))

(define-syntax suite-loader
  (lambda (stx)
    (syntax-case stx (metadata)
      ((_ suite-description (quote metadata) metadata-value
          expression expressions ...)
       (with-syntax ((location (datum->syntax
                                stx
                                (make-source-absolute (syntax-source stx)))))
         #'(let* ((suite-entity
                   `((suite/body-thunk . ,(lambda () expression expressions ...))
                     (suite/description . ,suite-description)
                     (suite/metadata . ,metadata-value)
                     (suite/location . location)))

                  (%suite-loader
                   ;; Wrapping into identity to prevent setting procedure-name
                   (identity
                    (lambda ()
                      ((test-runner*)
                       `((type . runner/load-suite)
                         (suite . ,suite-entity)))))))

             (set-procedure-properties!
              %suite-loader
              `((documentation . ,suite-description)
                (suite . ,suite-entity)
                (suitbl-suite-loader? . #t)
                (suitbl-suite-thunk? . #t)))
             %suite-loader)))

      ((_ suite-description expression expressions ...)
       #'(suite-loader
          suite-description 'metadata '() expression expressions ...)))))

(define-syntax suite-thunk
  (lambda (stx)
    (syntax-case stx ()
      ((_ arguments ...)
       (with-syntax ((location (datum->syntax
                                stx
                                (make-source-absolute (syntax-source stx)))))
         #'(begin
             (warn-deprecated-suite-thunk 'location)
             (suite-loader arguments ...)))))))

(define-syntax suite
  (lambda (stx)
    "Test suite is a grouping unit, it allows to combine tests and other
test suites."
    (syntax-case stx ()
      ((_ suite-description arguments ...)
       #'((suite-loader suite-description arguments ...))))))

(define-syntax define-suite
  (lambda (stx)
    "Equivalent of (define-public NAME (suite-loader ...))."
    (syntax-case stx ()
      ((_ suite-name expression ...)
       #'(define-public suite-name
           (suite-loader (symbol->string 'suite-name) expression ...))))))
