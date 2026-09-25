;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (srfi srfi-269)
  #:export (current-test-runner
            set-default-test-runner!

            is
            testing
            metadata
            test test?
            test-loader
            suite suite?
            suite-loader suite-loader?

            define-suite))

;; There implementation was ported from the (suitbl definitions)



;;;
;;; Test Definitions API
;;;

(define (missing-test-runner message)
  (error "current-test-runner is not set" message))

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

(define (test? object)
  (and (list? object)
       (assoc-ref object 'test/body-procedure)
       (assoc-ref object 'test/description)))

(define (suite? object)
  (and (list? object)
       (assoc-ref object 'suite/body-thunk)
       (assoc-ref object 'suite/description)))

(define (suite-loader? object)
  (and (procedure? object)
       (procedure-property object 'srfi-269-suite-loader?)))

(define (make-source-absolute source)
  "Make the filename in a syntax SOURCE alist absolute.  If the
filename is relative, resolve it by searching %load-path at macro-expansion
time."
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

(define-syntax metadata
  (syntax-rules ()))

(define %current-assertion-context
  (make-parameter '()))

(define-syntax testing
  (syntax-rules ()
    ((_ description body body* ...)
     (let ((description* description))
       (parameterize
           ((%current-assertion-context
             (append (%current-assertion-context)
                     (list description*))))
         body body* ...)))))

(define-syntax is
  (lambda (stx)
    "A flexible assertion macro.  Its behavior is customizable by a test runner."
    (define (build-assertion stx assertion-form fields)
      (with-syntax ((location (datum->syntax
                               stx
                               (make-source-absolute (syntax-source stx))))
                    (assertion-form assertion-form)
                    ((assertion-field ...) fields))
        #'(let ((assertion-context (%current-assertion-context)))
            ((current-test-runner)
             `((type . runner/run-assertion)
               (assertion
                . ((assertion/body-thunk
                    . ,(lambda ()
                         (parameterize
                             ((%current-assertion-context assertion-context))
                           assertion-form)))
                   (assertion/body . assertion-form)
                   (assertion/context . ,assertion-context)
                   assertion-field ...
                   (assertion/location . location))))))))

    (syntax-case stx ()
      ((_ form description)
       (build-assertion stx
                        #'form
                        #'((assertion/description . ,description))))
      ((_ form)
       (build-assertion stx #'form #'())))))

(define-syntax test-loader
  (lambda (stx)
    (define (build-test-loader stx description metadata body-procedure)
      (with-syntax ((location (datum->syntax
                               stx
                               (make-source-absolute (syntax-source stx))))
                    (test-description description)
                    (metadata-value metadata)
                    (test-body-procedure body-procedure))
        #'(let ((test-entity
                 `((test/body-procedure . ,test-body-procedure)
                   (test/description . ,test-description)
                   (test/metadata . ,metadata-value)
                   (test/location . location))))
            (lambda* (#:optional (metadata '()))
              ((current-test-runner)
               `((type . runner/load-test)
                 (load/metadata . ,metadata)
                 (test . ,test-entity)))))))

    (syntax-case stx (metadata)
      ((_ test-description (context-name)
          (metadata metadata-value) expression expressions ...)
       (identifier? #'context-name)
       (build-test-loader stx
                          #'test-description
                          #'metadata-value
                          #'(lambda (context-name)
                              expression expressions ...)))

      ((_ test-description ()
          (metadata metadata-value) expression expressions ...)
       (build-test-loader stx
                          #'test-description
                          #'metadata-value
                          #'(lambda (%srfi-269-context)
                              expression expressions ...)))

      ((_ test-description (context-name) expression expressions ...)
       (identifier? #'context-name)
       #'(test-loader test-description (context-name)
           (metadata '()) expression expressions ...))

      ((_ test-description () expression expressions ...)
       #'(test-loader test-description ()
           (metadata '()) expression expressions ...)))))

(define-syntax test
  (lambda (stx)
    "Represent a logical unit of testing, usually containing assertions."
    (syntax-case stx ()
      ((_ test-description arguments ...)
       #'((test-loader test-description arguments ...))))))

(define-syntax suite-loader
  (lambda (stx)
    (syntax-case stx (metadata)
      ((_ suite-description (metadata metadata-value)
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
                   ;; Prevent Guile from assigning a procedure name.
                   (identity
                    (lambda* (#:optional (metadata '()))
                      ((current-test-runner)
                       `((type . runner/load-suite)
                         (load/metadata . ,metadata)
                         (suite . ,suite-entity)))))))

             (set-procedure-properties!
              %suite-loader
              `((documentation . ,suite-description)
                (suite . ,suite-entity)
                (srfi-269-suite-loader? . #t)))
             %suite-loader)))

      ((_ suite-description expression expressions ...)
       #'(suite-loader
          suite-description (metadata '()) expression expressions ...)))))

(define-syntax suite
  (lambda (stx)
    "Group tests and nested test suites."
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
