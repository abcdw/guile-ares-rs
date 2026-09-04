;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-library (srfi srfi-269)
  (cond-expand
   ;; Without this import the module will be empty and eval in Ares
   ;; won't work.
   (guile (import (guile)))
   (else (import (scheme base))))
  (import (scheme case-lambda)
          (scheme write)
          (srfi 229))
  (export current-test-runner
          set-default-test-runner!
          simple-test-runner

          is
          test test?
          test-loader
          suite suite?
          suite-loader suite-loader?

          define-suite)

  (begin
    (define (missing-test-runner message)
      (error "current-test-runner is not set" message))

    (define default-test-runner missing-test-runner)

    (define (run-with-default-test-runner message)
      (default-test-runner message))

    (define current-test-runner
      (make-parameter run-with-default-test-runner))

    (define (set-default-test-runner! runner)
      (let ((previous-runner default-test-runner))
        (set! default-test-runner runner)
        previous-runner))

    (define (alist-ref alist key)
      (cdr (assq key alist)))

    (define (simple-test-runner message)
      (case (alist-ref message 'type)
        ((runner/load-suite)
         (let ((suite (alist-ref message 'suite)))
           (display "suite: ")
           (display (alist-ref suite 'suite/description))
           (newline)
           ((alist-ref suite 'suite/body-thunk))))

        ((runner/load-test)
         (let ((test (alist-ref message 'test)))
           (display "  test: ")
           (display (alist-ref test 'test/description))
           (newline)
           ((alist-ref test 'test/body-procedure) '())))

        ((runner/run-assertion)
         (let* ((assertion (alist-ref message 'assertion))
                (value ((alist-ref assertion 'assertion/body-thunk))))
           (display "    ")
           (display (if value "PASS " "FAIL "))
           (write (alist-ref assertion 'assertion/body))
           (newline)
           value))

        (else
         (error "unknown SRFI-269 message" message))))

    (set-default-test-runner! simple-test-runner)

    (define (alist-contains? alist key)
      (and (assoc key alist) #t))

    (define (test? obj)
      (and (list? obj)
           (alist-contains? obj 'test/body-procedure)
           (alist-contains? obj 'test/description)))

    (define (suite? obj)
      (and (list? obj)
           (alist-contains? obj 'suite/body-thunk)
           (alist-contains? obj 'suite/description)))

    (define suite-loader-tag-key (list 'suite-loader?))

    (define (make-suite-loader-tag suite-entity)
      (cons suite-loader-tag-key suite-entity))

    (define (suite-loader? obj)
      (and (procedure? obj)
           (procedure/tag? obj)
           (let ((tag (procedure-tag obj)))
             (and (pair? tag)
                  (eq? suite-loader-tag-key (car tag))))))

    (define (load-test test-entity metadata)
      ((current-test-runner)
       (list (cons 'type 'runner/load-test)
             (cons 'load/metadata metadata)
             (cons 'test test-entity))))

    (define (load-suite suite-entity metadata)
      ((current-test-runner)
       (list (cons 'type 'runner/load-suite)
             (cons 'load/metadata metadata)
             (cons 'suite suite-entity))))



    (define-syntax is
      (syntax-rules ()
        ((_ form description)
         ((current-test-runner)
          (list (cons 'type 'runner/run-assertion)
                (cons 'assertion
                      (list
                       (cons 'assertion/body-thunk
                             (lambda () form))
                       (cons 'assertion/body (quote form))
                       (cons 'assertion/description description)
                       (cons 'assertion/location #f))))))
        ((_ form)
         ((current-test-runner)
          (list (cons 'type 'runner/run-assertion)
                (cons 'assertion
                      (list
                       (cons 'assertion/body-thunk
                             (lambda () form))
                       (cons 'assertion/body (quote form))
                       (cons 'assertion/location #f))))))))

    (define-syntax test-loader
      (syntax-rules (quote metadata)
        ((_ test-description (context)
            (quote metadata) metadata-value body body* ...)
         (let ((test-entity
                (list
                 (cons 'test/body-procedure
                       (lambda (context) body body* ...))
                 (cons 'test/body (quote (body body* ...)))
                 (cons 'test/description test-description)
                 (cons 'test/metadata metadata-value)
                 (cons 'test/location #f))))
           (case-lambda
             (()
              (load-test test-entity '()))
             ((metadata)
              (load-test test-entity metadata)))))
        ((_ test-description ()
            (quote metadata) metadata-value body body* ...)
         (test-loader test-description (%test-context)
           'metadata metadata-value body body* ...))
        ((_ test-description (context) body body* ...)
         (test-loader test-description (context)
           'metadata '() body body* ...))
        ((_ test-description () body body* ...)
         (test-loader test-description ()
           'metadata '() body body* ...))))

    (define-syntax test
      (syntax-rules ()
        ((_ test-description arguments ...)
         ((test-loader test-description arguments ...)))))

    (define-syntax suite-loader
      (syntax-rules (quote metadata)
        ((_ suite-description (quote metadata) metadata-value body ...)
         (let ((suite-entity
                (list
                 (cons 'suite/body-thunk
                       (lambda () body ... (if #f #f)))
                 (cons 'suite/description suite-description)
                 (cons 'suite/metadata metadata-value)
                 (cons 'suite/location #f))))
           (case-lambda/tag (make-suite-loader-tag suite-entity)
             (()
              (load-suite suite-entity '()))
             ((metadata)
              (load-suite suite-entity metadata)))))
        ((_ suite-description body ...)
         (suite-loader suite-description 'metadata '() body ...))))

    (define-syntax suite
      (syntax-rules ()
        ((_ suite-description body ...)
         ((suite-loader suite-description body ...)))))

    (define-syntax define-suite
      (syntax-rules ()
        ((_ (suite-name) body ...)
         (define suite-name
           (suite-loader (symbol->string 'suite-name) body ...)))))))
