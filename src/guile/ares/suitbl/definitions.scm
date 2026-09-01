;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl definitions)
  #:export (current-test-runner
            set-default-test-runner!

            is
            test test?
            test-loader
            suite suite?
            suite-loader suite-loader?

            define-suite))



;;;
;;; Test Definitions API
;;;

(define (missing-test-runner _)
  (format (current-error-port) "\
The current-test-runner is not set. Probably you imported test defining API
directly instead of using a downstream testing library. That's
probably not what you want, unless you are a developer of a testing
library and enjoy seeing this message. Please, use suitbl or other
library, which sets an approriate test runner for you."))

(define default-test-runner missing-test-runner)

(define (run-with-default-test-runner message)
  (default-test-runner message))

(define current-test-runner
  (make-parameter run-with-default-test-runner))

(define (set-default-test-runner! runner)
  "Set the default test runner to RUNNER and return the previous runner."
  (let ((previous-runner default-test-runner))
    (set! default-test-runner runner)
    previous-runner))

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
       (procedure-property x 'suitbl-suite-loader?)))

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
        #'((current-test-runner)
           `((type . runner/run-assertion)
             (assertion . (assertion-field ...
                           (assertion/location . location)))))))

    (syntax-case stx ()
      ((_ form description)
       (build-assertion stx
                        #'((assertion/body-thunk . ,(lambda () form))
                           (assertion/body . form)
                           (assertion/description . ,description))))
      ((_ form)
       (build-assertion stx
                        #'((assertion/body-thunk . ,(lambda () form))
                           (assertion/body . form)))))))

(define (alist-merge l1 l2)
  (append l1 l2))

(define (amend-entity-metadata entity metadata-key metadata)
  (map (lambda (entry)
         (if (eq? metadata-key (car entry))
             (cons metadata-key
                   (alist-merge metadata (cdr entry)))
             entry))
       entity))

(define-syntax test-loader
  (lambda (stx)
    (define (build-test-loader stx description metadata body-procedure body)
      (with-syntax ((location (datum->syntax
                               stx
                               (make-source-absolute (syntax-source stx))))
                    (test-description description)
                    (metadata-value metadata)
                    (test-body-procedure body-procedure)
                    ((test-body ...) body))
        #'(let ((test-entity
                 `((test/body-procedure . ,test-body-procedure)
                   (test/body . (test-body ...))
                   (test/description . ,test-description)
                   (test/metadata . ,metadata-value)
                   (test/location . location))))
            (lambda* (#:optional (metadata '()))
              ((current-test-runner)
               `((type . runner/load-test)
                 (test . ,(amend-entity-metadata
                           test-entity 'test/metadata metadata))))))))

    (syntax-case stx (metadata)
      ((_ test-description (context-name)
          (quote metadata) metadata-value expression expressions ...)
       (identifier? #'context-name)
       (build-test-loader stx
                          #'test-description
                          #'metadata-value
                          #'(lambda (context-name)
                              expression expressions ...)
                          #'(expression expressions ...)))

      ((_ test-description ()
          (quote metadata) metadata-value expression expressions ...)
       (build-test-loader stx
                          #'test-description
                          #'metadata-value
                          #'(lambda (%suitbl-context)
                              expression expressions ...)
                          #'(expression expressions ...)))

      ((_ test-description (context-name) expression expressions ...)
       (identifier? #'context-name)
       #'(test-loader test-description (context-name)
           'metadata '() expression expressions ...))

      ((_ test-description () expression expressions ...)
       #'(test-loader test-description ()
           'metadata '() expression expressions ...)))))

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
                    (lambda* (#:optional (metadata '()))
                      ((current-test-runner)
                       `((type . runner/load-suite)
                         (suite . ,(amend-entity-metadata
                                    suite-entity 'suite/metadata
                                    metadata))))))))

             (set-procedure-properties!
              %suite-loader
              `((documentation . ,suite-description)
                (suite . ,suite-entity)
                (suitbl-suite-loader? . #t)))
             %suite-loader)))

      ((_ suite-description expression expressions ...)
       #'(suite-loader
          suite-description 'metadata '() expression expressions ...)))))

(define-syntax suite
  (lambda (stx)
    "Test suite is a grouping unit, it allows to combine tests and other
test suites."
    (syntax-case stx ()
      ((_ suite-description arguments ...)
       #'((suite-loader suite-description arguments ...))))))

(define-syntax define-suite
  (lambda (stx)
    "Define a public suite loader named NAME."
    (syntax-case stx ()
      ((_ (suite-name) expression ...)
       (identifier? #'suite-name)
       #'(define-public suite-name
           (suite-loader (symbol->string 'suite-name) expression ...))))))
