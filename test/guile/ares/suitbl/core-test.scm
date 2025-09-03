;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl core-test)
  #:use-module ((ares atomic) #:select (atomic-box-update!))
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl core)
  #:use-module (srfi srfi-197)
  #:use-module (ice-9 exceptions)
  #:use-module ((ice-9 atomic)
                #:select (make-atomic-box atomic-box-ref atomic-box-set!)))



(define (get-logging-test-runner)
  (define state (make-atomic-box '()))
  (lambda (message)
    (define msg-type (assoc-ref message 'type))
    (unless (equal? msg-type 'get-log)
      (atomic-box-update! state (lambda (l) (cons message l))))
    (case msg-type
      ((get-log)
       (reverse (atomic-box-ref state))))))

(define (simplify-log-entry entry)
  (define type (assoc-ref entry 'type))
  (case type
    ((load-suite)
     (chain entry
       (assoc-ref _ 'suite)
       (assoc-ref _ 'suite/description)))
    ((load-test)
     (chain entry
       (assoc-ref _ 'test)
       (assoc-ref _ 'test/description)))
    (else
     (chain entry
       (assoc-ref _ 'assert)
       (assoc-ref _ 'assert/body)))))

(define (simplify-log log)
  (map simplify-log-entry log))

(define-syntax with-runner-events-to-list
  (lambda (stx)
    (syntax-case stx ()
      ((_ body body* ...)
       #'(parameterize ((test-runner*
                         (get-logging-test-runner)))
           body body* ...
           ((test-runner*)
            `((type . get-log))))))))

(define-suite predicates-tests
  (test "test? predicate recognizes test structures"
    (is (test? `((test/body-thunk . ,(lambda () #t))
                 (test/description . "test"))))
    (is (not (test? '())))
    (is (not (test? `((test/body-thunk . ,(lambda () #t))))))
    (is (not (test? '((test/description . "hi"))))))

  (test "suite? predicate recognizes suite structures"
    (is (suite? `((suite/body-thunk . ,(lambda () #t))
                  (suite/description . "suite"))))
    (is (not (suite? '())))
    (is (not (suite? `((suite/body-thunk . ,(lambda () #t))))))
    (is (not (suite? '((suite/description . "suite"))))))

  (test "suite-thunk? identifies suite thunks"
    (define s (suite-thunk "test-suite" #t))
    (is (suite-thunk? s))
    (is (not (suite-thunk? (lambda () #t))))))

(define-suite definitions-to-runner-integration-tests
  (test "is emits proper values to the test runner"
    (define events-log
      (with-runner-events-to-list
       (is "a1")
       (is "a2")))
    (is (equal? '("a1" "a2") (simplify-log events-log))))

  (test "test emits proper values to the test runner"
    (define events-log
      (with-runner-events-to-list
       (test "t1" 'body)
       (test "t2" 'metadata '((good? . #t)) 'body)))
    (define (is-good? test)
      (chain test
          (assoc-ref _ 'test)
          (assoc-ref _ 'test/metadata)
          (assoc-ref _ 'good?)))
    (is (equal? '("t1" "t2") (simplify-log events-log)))
    (is (is-good? (cadr events-log))))

  (test "suite emits proper values to the test runner"
    (define events-log
      (with-runner-events-to-list
       (suite "s1" 'body)
       (suite "s2" 'metadata '((tags . (integration))) 'body)))
    (define (get-tags suite)
      (chain suite
        (assoc-ref _ 'suite)
        (assoc-ref _ 'suite/metadata)
        (assoc-ref _ 'tags)))
    (is (equal? '("s1" "s2") (simplify-log events-log)))
    (is (equal? '(integration) (get-tags (cadr events-log)))))

  (test "define-suite creates named suite thunk"
    (define tmp-suite-thunk
      (suite-thunk "tmp suite thunk" #t))
    (is (suite-thunk? tmp-suite-thunk))
    (is (not (suite-thunk? (lambda () #t))))))

(define-suite documentation-tests
  (test "exception, when macro used in place of predicate"
    ;; Due to the way macros work, if you use `chain' or similiar
    ;; macro in `is' assert, it will throw a quite unexpected
    ;; exception.  This happens because `is' macro extracts a list of
    ;; arguments to a separate thunk for better reporting in case of
    ;; error.  This thunk is supposed to be evaluated, when the
    ;; assertion fails to provide more clue to the user, however it
    ;; means that those arguments will be placed in the context, where
    ;; "predicate" doesn't exists and doesn't wrap them.
    (define exception
      (with-exception-handler
       (lambda (ex) ex)
       (lambda ()
         (with-runner-events-to-list
          ;; We have to use eval, otherwise this code won't compile
          (eval
           '(begin
              (use-modules (srfi srfi-197) (ares suitbl core))
              (is (chain 'hi (list _))))
           (interaction-environment))))
       #:unwind? #t))
    (pk exception)
    (is (equal? "bad use of '_' syntactic keyword"
                (exception-message exception)))))
