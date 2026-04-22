;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl definitions-test)
  #:use-module ((ares atomic) #:select (atomic-box-update!))
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl definitions)
  #:use-module ((ares suitbl reporters) #:prefix reporter:)
  #:use-module ((ares suitbl runner) #:prefix runner:)
  #:use-module ((ares suitbl state)
                #:prefix state:)
  #:use-module (srfi srfi-197)
  #:use-module (ice-9 exceptions)
  #:use-module ((ice-9 atomic)
                #:select (make-atomic-box atomic-box-ref atomic-box-set!)))



(define (get-logging-test-runner)
  (define state (make-atomic-box '()))
  (lambda (message)
    (define msg-type (assoc-ref message 'type))
    (unless (equal? msg-type 'runner/get-log)
      (atomic-box-update! state (lambda (l) (cons message l))))
    (case msg-type
      ((runner/get-log)
       (reverse (atomic-box-ref state))))))

(define (simplify-log-entry entry)
  (define type (assoc-ref entry 'type))
  (case type
    ((runner/load-suite)
     (chain entry
       (assoc-ref _ 'suite)
       (assoc-ref _ 'suite/description)))
    ((runner/load-test)
     (chain entry
       (assoc-ref _ 'test)
       (assoc-ref _ 'test/description)))
    (else
     (chain entry
       (assoc-ref _ 'assertion)
       (assoc-ref _ 'assertion/body)))))

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
            `((type . runner/get-log))))))))

(define (load-tests thunk)
  (define tr
    (runner:make-suitbl
     #:config `((auto-run? . #f)
                (test-reporter . ,reporter:silent))))
  (with-test-runner tr
    (thunk))
  (state:get-loaded-tests
   (runner:get-state tr)))

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
       (define str "a1")
       (is str)
       (is (= 1 (+ 2 -1)))))

    (is (equal? '(str (= 1 (+ 2 -1))) (simplify-log events-log)))

    (let* ((assertion-1 (chain events-log (car _) (assoc-ref _ 'assertion)))
           (assertion-1-body (assoc-ref assertion-1 'assertion/body))
           (assertion-1-body-value
            ((assoc-ref assertion-1 'assertion/body-thunk))))
      (is (equal? 'str assertion-1-body))
      (is (equal? "a1" assertion-1-body-value)))

    (let* ((assertion-2 (chain events-log (cadr _) (assoc-ref _ 'assertion)))
           (assertion-2-body (assoc-ref assertion-2 'assertion/body))
           (assertion-2-body-value
            ((assoc-ref assertion-2 'assertion/body-thunk)))
           (assertion-2-args-value
            ((assoc-ref assertion-2 'assertion/args-thunk))))
      (is (equal? '(= 1 (+ 2 -1)) assertion-2-body))
      (is (equal? #t assertion-2-body-value))
      (is (equal? '(1 1) assertion-2-args-value))))

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

  (test "runner adds compound metadata inherited from suite"
    (define compound-metadata
      (chain (suite-thunk "outer" 'metadata '((slow? . #t))
               (test "t1"
                 (is #t)))
        (load-tests _)
        (car _)
        (assoc-ref _ 'test/compound-metadata)))
    (is (equal? '((slow? . #t))
                compound-metadata)))

  (test "runner merges compound metadata from nested suites and test"
    (define compound-metadata
      (chain (suite-thunk "outer"
               'metadata
               '((shared . outer)
                 (outer? . #t))
               (suite "inner"
                 'metadata
                 '((shared . inner)
                   (inner? . #t))
                 (test "t1"
                   'metadata
                   '((shared . test)
                     (test? . #t))
                   (is #t))))
        (load-tests _)
        (car _)
        (assoc-ref _ 'test/compound-metadata)))
    (is (equal? '((shared . test)
                  (test? . #t)
                  (shared . inner)
                  (inner? . #t)
                  (shared . outer)
                  (outer? . #t))
                compound-metadata))
    (is (equal? 'test
                (assoc-ref compound-metadata 'shared))))

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
    (is (equal? "bad use of '_' syntactic keyword"
                (exception-message exception)))))
