;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-library (srfi srfi-269-draft)
  (cond-expand
   ;; Without this import the module will be empty and eval in Ares
   ;; won't work.
   (guile (import (guile)))
   (else (import (scheme base))))
  (import (srfi 229))
  (export current-test-runner
          set-current-test-runner!

          is
          test test?
          test-thunk
          suite suite?
          suite-thunk suite-thunk?

          define-suite)

  (begin
    (define (missing-test-runner message)
      (error "current-test-runner is not set" message))

    (define current-test-runner
      (make-parameter missing-test-runner))

    (define (set-current-test-runner! runner)
      (let ((previous-runner (current-test-runner)))
        ;; For Scheme implementations not supporting setting of a parameter,
        ;; the initial value can be an atomic box and this function can set the
        ;; atomic box value instead of the parameter itself.
        (current-test-runner runner)
        previous-runner))

    (define (alist-contains? alist key)
      (and (assoc key alist) #t))

    (define (test? obj)
      (and (list? obj)
           (alist-contains? obj 'test/body-thunk)
           (alist-contains? obj 'test/description)))

    (define (suite? obj)
      (and (list? obj)
           (alist-contains? obj 'suite/body-thunk)
           (alist-contains? obj 'suite/description)))

    (define suite-thunk-tag-key (list 'suite-thunk?))

    (define (make-suite-thunk-tag suite-entity)
      (cons suite-thunk-tag-key suite-entity))

    (define (suite-thunk? obj)
      (and (procedure? obj)
           (procedure/tag? obj)
           (let ((tag (procedure-tag obj)))
             (and (pair? tag)
                  (eq? suite-thunk-tag-key (car tag))))))



    (define-syntax is
      (syntax-rules ()
        ((_ (predicate argument ...))
         ((current-test-runner)
          (list (cons 'type 'runner/run-assert)
                (cons 'assertion
                      (list
                       (cons 'assertion/body-thunk
                             (lambda () (predicate argument ...)))
                       (cons 'assertion/args-thunk
                             (lambda () (list argument ...)))
                       (cons 'assertion/body
                             (quote (predicate argument ...)))
                       (cons 'assertion/location #f))))))
        ((_ form)
         ((current-test-runner)
          (list (cons 'type 'runner/run-assert)
                (cons 'assertion
                      (list
                       (cons 'assertion/body-thunk
                             (lambda () form))
                       (cons 'assertion/body (quote form))
                       (cons 'assertion/location #f))))))))

    (define-syntax test-thunk
      (syntax-rules (quote metadata)
        ((_ test-description (quote metadata) metadata-value body ...)
         (let ((test-entity
                (list
                 (cons 'test/body-thunk
                       (lambda () body ... (if #f #f)))
                 (cons 'test/body (quote (body ...)))
                 (cons 'test/description test-description)
                 (cons 'test/metadata metadata-value)
                 (cons 'test/location #f))))
           (lambda ()
             ((current-test-runner)
              (list (cons 'type 'runner/load-test)
                    (cons 'test test-entity))))))
        ((_ test-description body ...)
         (test-thunk test-description 'metadata '() body ...))))

    (define-syntax test
      (syntax-rules ()
        ((_ test-description body ...)
         ((test-thunk test-description body ...)))))

    (define-syntax suite-thunk
      (syntax-rules (quote metadata)
        ((_ suite-description (quote metadata) metadata-value body ...)
         (let ((suite-entity
                (list
                 (cons 'suite/body-thunk
                       (lambda () body ... (if #f #f)))
                 (cons 'suite/description suite-description)
                 (cons 'suite/metadata metadata-value)
                 (cons 'suite/location #f))))
           (lambda/tag (make-suite-thunk-tag suite-entity)
             ()
             ((current-test-runner)
              (list (cons 'type 'runner/load-suite)
                    (cons 'suite suite-entity))))))
        ((_ suite-description body ...)
         (suite-thunk suite-description 'metadata '() body ...))))

    (define-syntax suite
      (syntax-rules ()
        ((_ suite-description body ...)
         ((suite-thunk suite-description body ...)))))

    ;; TODO: [Andrew Tropin, 2026-04-14] There is no define-public in
    ;; r7rs, update SRFI?
    (define-syntax define-suite
      (syntax-rules ()
        ((_ suite-name body ...)
         (define suite-name
           (suite-thunk (symbol->string 'suite-name) body ...)))))))
