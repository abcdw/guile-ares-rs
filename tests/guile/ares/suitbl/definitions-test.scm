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
       #'(parameterize ((current-test-runner
                         (get-logging-test-runner)))
           body body* ...
           ((current-test-runner)
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

(define (eval-suite-definition expression suite-name)
  (define module (make-fresh-user-module))
  (module-use! module (resolve-interface '(ares suitbl core)))
  (eval expression module)
  (module-ref module suite-name))

(define-suite (predicates-tests)
  (test "test? predicate recognizes test structures" ()
    (is (test? `((test/body-procedure . ,(lambda (_) #t))
                 (test/description . "test"))))
    (is (not (test? '())))
    (is (not (test? `((test/body-procedure . ,(lambda (_) #t))))))
    (is (not (test? '((test/description . "hi"))))))

  (test "suite? predicate recognizes suite structures" ()
    (is (suite? `((suite/body-thunk . ,(lambda () #t))
                  (suite/description . "suite"))))
    (is (not (suite? '())))
    (is (not (suite? `((suite/body-thunk . ,(lambda () #t))))))
    (is (not (suite? '((suite/description . "suite"))))))

  (test "suite-loader? identifies suite loaders" ()
    (define s (suite-loader "test-suite" #t))
    (is (suite-loader? s))
    (is (not (suite-loader? (lambda () #t))))))

(define-suite (test-runner-parameter-tests)
  (test "parameterized runner takes precedence over changes to the default" ()
    (define default-runner (lambda (_) 'default))
    (define updated-runner (lambda (_) 'updated))
    (define dynamic-runner (lambda (_) 'dynamic))

    (define original-default (set-default-test-runner! default-runner))

    (define result
      (parameterize ((current-test-runner dynamic-runner))
        (set-default-test-runner! updated-runner)
        ((current-test-runner) 'message)))

    (define previous-default
      (set-default-test-runner! original-default))

    (is
     (eq? 'dynamic result)
     "the dynamically parameterized runner takes precedence over set-default")

    (is (eq? updated-runner previous-default)
        "the default runner was updated")))

(define-suite (definitions-to-runner-integration-tests)
  (test "is emits proper values to the test runner" ()
    (define events-log
      (with-runner-events-to-list
       (define str "a1")
       (is str)
       (is (= 1 (+ 2 -1)))
       (is str "string assertion")
       (is (= 2 (+ 1 1)) "described assertion")))

    (is (equal? '(str
                  (= 1 (+ 2 -1))
                  str
                  (= 2 (+ 1 1)))
                (simplify-log events-log)))

    (let* ((assertion-1 (chain events-log (car _) (assoc-ref _ 'assertion)))
           (assertion-1-body (assoc-ref assertion-1 'assertion/body))
           (assertion-1-body-thunk
            (assoc-ref assertion-1 'assertion/body-thunk)))
      (is (equal? 'str assertion-1-body))
      (is (procedure? assertion-1-body-thunk))
      (is (not (assoc-ref assertion-1 'assertion/description)))
      (is (not (assoc-ref assertion-1 'assertion/args-thunk))))

    (let* ((assertion-2 (chain events-log (cadr _) (assoc-ref _ 'assertion)))
           (assertion-2-body (assoc-ref assertion-2 'assertion/body))
           (assertion-2-body-thunk
            (assoc-ref assertion-2 'assertion/body-thunk)))
      (is (equal? '(= 1 (+ 2 -1)) assertion-2-body))
      (is (procedure? assertion-2-body-thunk))
      (is (not (assoc-ref assertion-2 'assertion/description)))
      (is (not (assoc-ref assertion-2 'assertion/args-thunk))))

    (let* ((assertion-3 (chain events-log (caddr _) (assoc-ref _ 'assertion)))
           (assertion-4 (chain events-log (cadddr _) (assoc-ref _ 'assertion))))
      (is (equal? "string assertion"
                  (assoc-ref assertion-3 'assertion/description)))
      (is (not (assoc-ref assertion-3 'assertion/args-thunk)))
      (is (equal? "described assertion"
                  (assoc-ref assertion-4 'assertion/description)))
      (is (not (assoc-ref assertion-4 'assertion/args-thunk)))))

  (test "test emits proper values to the test runner" ()
    (define events-log
      (with-runner-events-to-list
       (test "t1" () 'body)
       (test "t2" () 'metadata '((good? . #t)) 'body)
       (test "t3" (ctx) (assoc-ref ctx 'answer))))
    (define (event-test event)
      (assoc-ref event 'test))
    (define test-1 (event-test (car events-log)))
    (define test-2 (event-test (cadr events-log)))
    (define test-3 (event-test (caddr events-log)))
    (define (is-good? test)
      (chain test
        (assoc-ref _ 'test/metadata)
        (assoc-ref _ 'good?)))
    (is (equal? '("t1" "t2" "t3") (simplify-log events-log)))
    (is (procedure? (assoc-ref test-1 'test/body-procedure)))
    (is (equal? 'body
                ((assoc-ref test-1 'test/body-procedure) '())))
    (is (is-good? test-2))
    (is (equal? 'value
                ((assoc-ref test-3 'test/body-procedure)
                 '((answer . value))))))

  (test "test-loader emits call metadata separately" ()
    (define tmp-test-loader
      (test-loader "tmp test loader" ()
        'metadata
        '((default? . #t)
          (shared . default))
        #t))

    (define load-event
      (car (with-runner-events-to-list
            (tmp-test-loader '((added? . #t)
                               (shared . amended))))))

    (is (equal?
         '((default? . #t)
           (shared . default))
         (chain load-event
           (assoc-ref _ 'test)
           (assoc-ref _ 'test/metadata))))
    (is (equal?
         '((added? . #t)
           (shared . amended))
         (assoc-ref load-event 'load/metadata))))

  (test "runner adds compound metadata inherited from suite" ()
    (define compound-metadata
      (chain (suite-loader "outer" 'metadata '((slow? . #t))
               (test "t1" ()
                 (is #t)))
        (load-tests _)
        (car _)
        (assoc-ref _ 'test/compound-metadata)))
    (is (equal? '((slow? . #t))
                compound-metadata)))

  (test "runner merges compound metadata from nested suites and test" ()
    (define compound-metadata
      (chain (suite-loader "outer"
               'metadata
               '((shared . outer)
                 (outer? . #t))
               (suite "inner"
                 'metadata
                 '((shared . inner)
                   (inner? . #t))
                 (test "t1" ()
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

  (test "suite emits proper values to the test runner" ()
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

  (test "suite-loader emits call metadata separately" ()
    (define tmp-suite-loader
      (suite-loader "tmp suite loader"
        'metadata
        '((default? . #t)
          (shared . default))
        #t))

    (define load-event
      (car (with-runner-events-to-list
            (tmp-suite-loader '((added? . #t)
                                (shared . amended))))))

    (is (suite-loader? tmp-suite-loader))
    (is (not (suite-loader? (lambda () #t))))

    (is (equal? '((default? . #t)
                  (shared . default))
                (chain load-event
                  (assoc-ref _ 'suite)
                  (assoc-ref _ 'suite/metadata))))
    (is (equal? '((added? . #t)
                  (shared . amended))
                (assoc-ref load-event 'load/metadata))))

  (test "define-suite creates suite loader with parenthesized syntax" ()
    (define generated-suite
      (eval-suite-definition
       '(define-suite (generated-suite) #t)
       'generated-suite))
    (is (suite-loader? generated-suite))
    (is (equal? "generated-suite"
                (assoc-ref (procedure-property generated-suite 'suite)
                           'suite/description)))))

(define-suite (macro-inside-assertion-tests)
  (test "macro forms can be used as assertion bodies" ()
    (define test-runner
      (runner:make-suitbl
       #:config `((test-reporter . ,reporter:silent))))
    (parameterize ((current-test-runner test-runner))
      (suite "macro assertion sample"
        (test "chain + assertion" ()
          (is (chain 'hi (list _)))
          (chain (+ 2 2)
            (= 4 _)
            (is _ "#t from macro bound _ identifier"))))
      (test-runner `((type . runner/run-tests))))
    (define summary
      (state:get-run-summary
       (runner:get-state test-runner)))
    (is (= 1 (assoc-ref summary 'tests)))
    (is (= 2 (assoc-ref summary 'assertions)))
    (is (= 0 (assoc-ref summary 'failures)))
    (is (= 0 (assoc-ref summary 'errors)))))
