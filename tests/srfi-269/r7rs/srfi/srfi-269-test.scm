;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-library (srfi srfi-269-test)
  (import (scheme base)
          (guile)
          (srfi srfi-64)
          (prefix (srfi 269) t:))
  (export srfi-269)

  (begin
    (define-syntax define-test
      (syntax-rules ()
        ((_ test-name e ...)
         (begin
           (define (test-name) e ...)
           (set-procedure-property! test-name 'srfi-64-test? #t)))))

    (define (alist-ref alist key)
      (let ((entry (assq key alist)))
        (and entry (cdr entry))))

    (define (make-logging-runner)
      (let ((events '()))
        (lambda (message)
          (let ((message-type (alist-ref message 'type)))
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
        (test-assert "set-default-test-runner! installs a runner and returns the previous runner"
          (let ((first-runner (lambda (message) (cons 'first message)))
                (second-runner (lambda (message) (cons 'second message)))
                (original-runner #f)
                (previous-runner #f)
                (result #f))

            (set! original-runner (t:set-default-test-runner! first-runner))
            (set! previous-runner (t:set-default-test-runner! second-runner))
            (set! result ((t:current-test-runner) '(message)))
            (t:set-default-test-runner! original-runner)

            (and (eq? first-runner previous-runner)
                 (equal? '(second message) result))))

        (test-assert "parameterize overrides changes to the default runner"
          (let ((default-runner (lambda (message) (cons 'default message)))
                (updated-runner (lambda (message) (cons 'updated message)))
                (override-runner (lambda (message) (cons 'override message)))
                (original-runner #f)
                (inside-result #f)
                (outside-result #f))

            (set! original-runner (t:set-default-test-runner! default-runner))
            (parameterize ((t:current-test-runner override-runner))
              (t:set-default-test-runner! updated-runner)
              (set! inside-result ((t:current-test-runner) '(message))))
            (set! outside-result ((t:current-test-runner) '(message)))

            (t:set-default-test-runner! original-runner)

            (and (equal? '(override message) inside-result)
                 (equal? '(updated message) outside-result))))

        (test-assert "test? recognizes test entities"
          (t:test? `((test/body-procedure . ,(lambda (context) #t))
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
                 (assertion (alist-ref message 'assertion))
                 (described-assertion
                  (alist-ref (cadr events) 'assertion)))
            (test-equal "message type"
              'runner/run-assertion
              (alist-ref message 'type))
            (test-equal "assertion body datum"
              '(= 42 (+ x 1))
              (alist-ref assertion 'assertion/body))
            (test-assert "assertion without description omits description field"
              (not (alist-contains-key? assertion 'assertion/description)))
            (test-equal "assertion location"
              #f
              (alist-ref assertion 'assertion/location))
            (test-equal "body thunk value"
              #t
              ((alist-ref assertion 'assertion/body-thunk)))
            (test-assert "generic assertions omit argument thunks"
              (not (alist-contains-key? assertion 'assertion/args-thunk)))
            (test-equal "described assertion body datum"
              '(and #t x)
              (alist-ref described-assertion 'assertion/body))
            (test-equal "assertion description"
              "x is true"
              (alist-ref described-assertion 'assertion/description))))

        (test-group "test"
          (let* ((events (runner-events
                          (lambda ()
                            (t:test "addition" ()
                              (t:metadata '((tag . unit)))
                              (define value 2)
                              (t:is (= 4 (+ value value))))
                            (t:test "context" (context)
                              (t:is (= 42
                                       (cdr (assq 'answer context))))))))
                 (message (car events))
                 (test-entity (alist-ref message 'test))
                 (context-test-entity
                  (alist-ref (cadr events) 'test)))
            (test-equal "message type"
              'runner/load-test
              (alist-ref message 'type))
            (test-equal "load metadata"
              '()
              (alist-ref message 'load/metadata))
            (test-equal "description"
              "addition"
              (alist-ref test-entity 'test/description))
            (test-equal "metadata"
              '((tag . unit))
              (alist-ref test-entity 'test/metadata))
            (test-equal "location"
              #f
              (alist-ref test-entity 'test/location))
            (test-assert "entity predicate"
              (t:test? test-entity))
            (test-assert "body procedure"
              (procedure? (alist-ref test-entity 'test/body-procedure)))
            (let* ((body-events
                    (runner-events
                     (lambda ()
                       ((alist-ref test-entity 'test/body-procedure) '()))))
                   (assertion (alist-ref (car body-events) 'assertion)))
              (test-equal "body procedure loads assertions"
                '(= 4 (+ value value))
                (alist-ref assertion 'assertion/body)))
            (let* ((body-events
                    (runner-events
                     (lambda ()
                       ((alist-ref context-test-entity 'test/body-procedure)
                        '((answer . 42))))))
                   (assertion (alist-ref (car body-events) 'assertion)))
              (test-equal "context is bound in the body procedure"
                #t
                ((alist-ref assertion 'assertion/body-thunk))))))

        (test-group "test-loader"
          (let ((test-loader #f))
            (test-equal "construction is deferred"
              '()
              (runner-events
               (lambda ()
                 (set! test-loader
                       (t:test-loader "deferred test" ()
                         (t:metadata
                          '((tag . test)
                            (shared . definition)))
                         #t)))))

            (test-assert "returns a procedure"
              (procedure? test-loader))

            (let* ((events (runner-events (lambda () (test-loader))))
                   (message (car events))
                   (test-entity (alist-ref message 'test)))
              (test-equal "message type"
                'runner/load-test
                (alist-ref message 'type))
              (test-equal "default load metadata"
                '()
                (alist-ref message 'load/metadata))
              (test-equal "description"
                "deferred test"
                (alist-ref test-entity 'test/description))
              (test-equal "definition-time metadata"
                '((tag . test)
                  (shared . definition))
                (alist-ref test-entity 'test/metadata))
              (test-equal "location"
                #f
                (alist-ref test-entity 'test/location))
              (test-assert "entity predicate"
                (t:test? test-entity)))

            (let* ((events
                    (runner-events
                     (lambda ()
                       (test-loader '((added? . #t)
                                      (shared . invocation))))))
                   (message (car events))
                   (test-entity (alist-ref message 'test)))
              (test-equal "call-time metadata is emitted separately"
                '((added? . #t)
                  (shared . invocation))
                (alist-ref message 'load/metadata))
              (test-equal "call-time metadata does not amend the entity"
                '((tag . test)
                  (shared . definition))
                (alist-ref test-entity 'test/metadata)))

            (let* ((events (runner-events (lambda () (test-loader))))
                   (test-entity (alist-ref (car events) 'test)))
              (test-equal "calls do not modify definition-time metadata"
                '((tag . test)
                  (shared . definition))
                (alist-ref test-entity 'test/metadata)))))

        (test-group "suite"
          (let ((suite-loader
                 (t:suite-loader "deferred"
                   (t:metadata
                    '((tag . suite)
                      (shared . definition)))
                   (t:test "inside" () #t))))
            (test-assert "suite-loader? recognizes suite loaders"
              (t:suite-loader? suite-loader))
            (test-assert "suite-loader? rejects ordinary procedures"
              (not (t:suite-loader? (lambda () #t))))
            (let* ((events (runner-events (lambda () (suite-loader))))
                   (message (car events))
                   (suite-entity (alist-ref message 'suite))
                   (amended-events
                    (runner-events
                     (lambda ()
                       (suite-loader '((added? . #t)
                                       (shared . invocation))))))
                   (amended-message (car amended-events))
                   (amended-suite-entity
                    (alist-ref amended-message 'suite))
                   (reloaded-events
                    (runner-events (lambda () (suite-loader))))
                   (reloaded-suite-entity
                    (alist-ref (car reloaded-events) 'suite)))
              (test-equal "message type"
                'runner/load-suite
                (alist-ref message 'type))
              (test-equal "default load metadata"
                '()
                (alist-ref message 'load/metadata))
              (test-equal "description"
                "deferred"
                (alist-ref suite-entity 'suite/description))
              (test-equal "definition-time metadata"
                '((tag . suite)
                  (shared . definition))
                (alist-ref suite-entity 'suite/metadata))
              (test-equal "call-time metadata is emitted separately"
                '((added? . #t)
                  (shared . invocation))
                (alist-ref amended-message 'load/metadata))
              (test-equal "call-time metadata does not amend the entity"
                '((tag . suite)
                  (shared . definition))
                (alist-ref amended-suite-entity 'suite/metadata))
              (test-equal "calls do not modify definition-time metadata"
                '((tag . suite)
                  (shared . definition))
                (alist-ref reloaded-suite-entity 'suite/metadata))
              (test-equal "location"
                #f
                (alist-ref suite-entity 'suite/location))
              (test-assert "entity predicate"
                (t:suite? suite-entity)))))

        (test-group "define-suite"
          (begin
            (t:define-suite (generated-suite)
              (t:test "inside generated suite" () #t))
            (test-assert "creates suite loader"
              (t:suite-loader? generated-suite))
            (let* ((events (runner-events (lambda () (generated-suite))))
                   (suite-entity (alist-ref (car events) 'suite)))
              (test-equal "generated description"
                "generated-suite"
                (alist-ref suite-entity 'suite/description)))))))))
