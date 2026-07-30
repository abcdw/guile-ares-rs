;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-library (srfi srfi-269-draft-test)
  (import (scheme base)
          (guile)
          (srfi srfi-64)
          (prefix (srfi srfi-269-draft) t:))
  (export srfi-269)

  (begin
    (define-syntax define-test
      (syntax-rules ()
        ((_ test-name e ...)
         (begin
           (define (test-name) e ...)
           (set-procedure-property! test-name 'srfi-64-test? #t)))))

    (define (make-logging-runner)
      (let ((events '()))
        (lambda (message)
          (let ((message-type (assoc-ref message 'type)))
            (if (eq? message-type 'runner/get-log)
                (reverse events)
                (begin
                  (set! events (cons message events))
                  *unspecified*))))))

    (define (runner-events thunk)
      (let ((runner (make-logging-runner)))
        (parameterize ((t:current-test-runner runner))
          (thunk)
          (runner '((type . runner/get-log))))))

    (define (alist-contains-key? alist key)
      (cond
       ((null? alist) #f)
       ((eq? key (caar alist)) #t)
       (else (alist-contains-key? (cdr alist) key))))

    (define-test srfi-269
      (test-group "srfi-269"
        (test-assert "set-current-test-runner! returns the previous runner"
          (let ((original-runner (t:current-test-runner))
                (new-runner (lambda (message) message))
                (previous-runner #f)
                (current-runner #f))
            (dynamic-wind
              (lambda () #t)
              (lambda ()
                (set! previous-runner (t:set-current-test-runner! new-runner))
                (set! current-runner (t:current-test-runner)))
              (lambda ()
                (t:set-current-test-runner! original-runner)))
            (and (eq? original-runner previous-runner)
                 (eq? new-runner current-runner)
                 (eq? original-runner (t:current-test-runner)))))

        (test-assert "test? recognizes test entities"
          (t:test? `((test/body-thunk . ,(lambda () #t))
                     (test/description . "test"))))

        (test-assert "suite? recognizes suite entities"
          (t:suite? `((suite/body-thunk . ,(lambda () #t))
                      (suite/description . "suite"))))

        (test-group "is"
          (let* ((events (runner-events
                          (lambda ()
                            (let ((x 41))
                              (t:is (= 42 (+ x 1)))
                              (t:is (and #t x) "x is true")))))
                 (message (car events))
                 (assertion (assoc-ref message 'assertion))
                 (described-assertion
                  (assoc-ref (cadr events) 'assertion)))
            (test-equal "message type"
              'runner/run-assertion
              (assoc-ref message 'type))
            (test-equal "assertion body datum"
              '(= 42 (+ x 1))
              (assoc-ref assertion 'assertion/body))
            (test-assert "assertion without description omits description field"
              (not (alist-contains-key? assertion 'assertion/description)))
            (test-equal "assertion location"
              #f
              (assoc-ref assertion 'assertion/location))
            (test-equal "body thunk value"
              #t
              ((assoc-ref assertion 'assertion/body-thunk)))
            (test-assert "generic assertions omit argument thunks"
              (not (alist-contains-key? assertion 'assertion/args-thunk)))
            (test-equal "described assertion body datum"
              '(and #t x)
              (assoc-ref described-assertion 'assertion/body))
            (test-equal "assertion description"
              "x is true"
              (assoc-ref described-assertion 'assertion/description))))

        (test-group "test"
          (let* ((events (runner-events
                          (lambda ()
                            (t:test "addition"
                              'metadata
                              '((tag . unit))
                              (define value 2)
                              (t:is (= 4 (+ value value)))))))
                 (message (car events))
                 (test-entity (assoc-ref message 'test)))
            (test-equal "message type"
              'runner/load-test
              (assoc-ref message 'type))
            (test-equal "description"
              "addition"
              (assoc-ref test-entity 'test/description))
            (test-equal "metadata"
              '((tag . unit))
              (assoc-ref test-entity 'test/metadata))
            (test-equal "location"
              #f
              (assoc-ref test-entity 'test/location))
            (test-assert "entity predicate"
              (t:test? test-entity))
            (test-assert "body thunk"
              (procedure? (assoc-ref test-entity 'test/body-thunk)))
            (let* ((body-events
                    (runner-events
                     (lambda ()
                       ((assoc-ref test-entity 'test/body-thunk)))))
                   (assertion (assoc-ref (car body-events) 'assertion)))
              (test-equal "body thunk loads assertions"
                '(= 4 (+ value value))
                (assoc-ref assertion 'assertion/body)))))

        (test-group "suite"
          (let ((suite-loader
                 (t:suite-thunk "deferred"
                   'metadata
                   '((tag . suite))
                   (t:test "inside" #t))))
            (test-assert "suite-thunk? recognizes suite thunks"
              (t:suite-thunk? suite-loader))
            (test-assert "suite-thunk? rejects ordinary procedures"
              (not (t:suite-thunk? (lambda () #t))))
            (let* ((events (runner-events (lambda () (suite-loader))))
                   (message (car events))
                   (suite-entity (assoc-ref message 'suite)))
              (test-equal "message type"
                'runner/load-suite
                (assoc-ref message 'type))
              (test-equal "description"
                "deferred"
                (assoc-ref suite-entity 'suite/description))
              (test-equal "metadata"
                '((tag . suite))
                (assoc-ref suite-entity 'suite/metadata))
              (test-equal "location"
                #f
                (assoc-ref suite-entity 'suite/location))
              (test-assert "entity predicate"
                (t:suite? suite-entity)))))

        (test-group "define-suite"
          (begin
            (t:define-suite generated-suite
              (t:test "inside generated suite" #t))
            (test-assert "creates suite thunk"
              (t:suite-thunk? generated-suite))
            (let* ((events (runner-events (lambda () (generated-suite))))
                   (suite-entity (assoc-ref (car events) 'suite)))
              (test-equal "generated description"
                "generated-suite"
                (assoc-ref suite-entity 'suite/description)))))))))
