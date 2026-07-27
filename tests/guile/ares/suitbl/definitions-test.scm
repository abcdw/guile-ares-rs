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
  (define generated-suite #f)
  (define warnings
    (call-with-output-string
     (lambda (port)
       (parameterize ((current-warning-port port))
         (module-use! module (resolve-interface '(ares suitbl core)))
         (eval expression module)
         (set! generated-suite (module-ref module suite-name))))))
  (values generated-suite warnings))

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
  (test "set-current-test-runner! changes the current test runner" ()
    (define original-runner (current-test-runner))
    (define new-runner (get-logging-test-runner))
    (define previous-runner #f)
    (define current-runner #f)
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (set! previous-runner (set-current-test-runner! new-runner))
        (set! current-runner (current-test-runner)))
      (lambda ()
        (set-current-test-runner! original-runner)))
    (is (eq? original-runner previous-runner))
    (is (eq? new-runner current-runner))
    (is (eq? original-runner (current-test-runner)))))

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

  (test "test-loader amends metadata when called" ()
    (define tmp-test-loader
      (test-loader "tmp test loader" ()
        'metadata
        '((default? . #t)
          (shared . default))
        #t))
    (define amended-metadata
      (chain (with-runner-events-to-list
              (tmp-test-loader '((added? . #t)
                                 (shared . amended))))
        (car _)
        (assoc-ref _ 'test)
        (assoc-ref _ 'test/metadata)))
    (is (equal? '((added? . #t)
                  (shared . amended)
                  (default? . #t)
                  (shared . default))
                amended-metadata))
    (is (equal? 'amended (assoc-ref amended-metadata 'shared))))

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

  (test "suite-loader creates named suite loader and amends metadata" ()
    (define tmp-suite-loader
      (suite-loader "tmp suite loader"
        'metadata
        '((default? . #t)
          (shared . default))
        #t))
    (define amended-metadata
      (chain (with-runner-events-to-list
              (tmp-suite-loader '((added? . #t)
                                  (shared . amended))))
        (car _)
        (assoc-ref _ 'suite)
        (assoc-ref _ 'suite/metadata)))
    (is (suite-loader? tmp-suite-loader))
    (is (not (suite-loader? (lambda () #t))))
    (is (equal? '((added? . #t)
                  (shared . amended)
                  (default? . #t)
                  (shared . default))
                amended-metadata))
    (is (equal? 'amended (assoc-ref amended-metadata 'shared))))

  (test "define-suite creates suite loader with parenthesized syntax" ()
    (call-with-values
     (lambda ()
       (eval-suite-definition
        '(define-suite (generated-suite) #t)
        'generated-suite))
     (lambda (generated-suite warnings)
       (is (suite-loader? generated-suite))
       (is (equal? "generated-suite"
                   (assoc-ref (procedure-property generated-suite 'suite)
                              'suite/description)))
       (is (string=? "" warnings)))))

  (test "define-suite warns for deprecated bare-name syntax" ()
    (call-with-values
     (lambda ()
       (eval-suite-definition
        '(define-suite deprecated-generated-suite #t)
        'deprecated-generated-suite))
     (lambda (generated-suite warnings)
       (is (suite-loader? generated-suite))
       (is (string-contains warnings
                            "warning: deprecated suitbl define-suite form"))
       (is (string-contains warnings
                            "`(define-suite NAME BODY ...)` syntax is deprecated."))))))

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
