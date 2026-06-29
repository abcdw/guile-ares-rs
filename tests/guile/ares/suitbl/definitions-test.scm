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
  (test ("test? predicate recognizes test structures" _)
    (is (test? `((test/body-procedure . ,(lambda (_) #t))
                 (test/description . "test"))))
    (is (not (test? '())))
    (is (not (test? `((test/body-procedure . ,(lambda (_) #t))))))
    (is (not (test? '((test/description . "hi"))))))

  (test ("suite? predicate recognizes suite structures" _)
    (is (suite? `((suite/body-thunk . ,(lambda () #t))
                  (suite/description . "suite"))))
    (is (not (suite? '())))
    (is (not (suite? `((suite/body-thunk . ,(lambda () #t))))))
    (is (not (suite? '((suite/description . "suite"))))))

  (test ("suite-loader? identifies suite loaders" _)
    (define s (suite-loader "test-suite" #t))
    (is (suite-loader? s))
    (is (not (suite-loader? (lambda () #t))))))

(define-suite (test-runner-parameter-tests)
  (test ("set-test-runner! changes the current test runner" _)
    (define original-runner (test-runner*))
    (define new-runner (get-logging-test-runner))
    (define previous-runner #f)
    (define current-runner #f)
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (set! previous-runner (set-test-runner! new-runner))
        (set! current-runner (test-runner*)))
      (lambda ()
        (set-test-runner! original-runner)))
    (is (eq? original-runner previous-runner))
    (is (eq? new-runner current-runner))
    (is (eq? original-runner (test-runner*)))))

(define-suite (definitions-to-runner-integration-tests)
  (test ("is emits proper values to the test runner" ctx)
    (define events-log
      (with-runner-events-to-list
       (define str "a1")
       (is str)
       (is (= 1 (+ 2 -1)))
       (is str "string assertion")
       (is (= 2 (+ 1 1)) "predicate assertion")))

    (is (equal? '(str
                  (= 1 (+ 2 -1))
                  str
                  (= 2 (+ 1 1)))
                (simplify-log events-log)))

    (let* ((assertion-1 (chain events-log (car _) (assoc-ref _ 'assertion)))
           (assertion-1-body (assoc-ref assertion-1 'assertion/body))
           (assertion-1-body-value
            ((assoc-ref assertion-1 'assertion/body-thunk))))
      (is (equal? 'str assertion-1-body))
      (is (not (assoc-ref assertion-1 'assertion/description)))
      (is (equal? "a1" assertion-1-body-value)))

    (let* ((assertion-2 (chain events-log (cadr _) (assoc-ref _ 'assertion)))
           (assertion-2-body (assoc-ref assertion-2 'assertion/body))
           (assertion-2-body-value
            ((assoc-ref assertion-2 'assertion/body-thunk)))
           (assertion-2-args-value
            ((assoc-ref assertion-2 'assertion/args-thunk))))
      (is (equal? '(= 1 (+ 2 -1)) assertion-2-body))
      (is (not (assoc-ref assertion-2 'assertion/description)))
      (is (equal? #t assertion-2-body-value))
      (is (equal? '(1 1) assertion-2-args-value)))

    (let* ((assertion-3 (chain events-log (caddr _) (assoc-ref _ 'assertion)))
           (assertion-4 (chain events-log (cadddr _) (assoc-ref _ 'assertion))))
      (is (equal? "string assertion"
                  (assoc-ref assertion-3 'assertion/description)))
      (is (equal? "predicate assertion"
                  (assoc-ref assertion-4 'assertion/description)))
      (is (equal? '(2 2)
                  ((assoc-ref assertion-4 'assertion/args-thunk))))))

  (test ("test emits proper values to the test runner" ctx)
    (define events-log
      (with-runner-events-to-list
       (test ("t1" _) 'body)
       (test ("t2" _) 'metadata '((good? . #t)) 'body)
       (test ("t3" ctx) (assoc-ref ctx 'answer))))
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

  (test ("runner adds compound metadata inherited from suite" ctx)
    (define compound-metadata
      (chain (suite-loader "outer" 'metadata '((slow? . #t))
               (test ("t1" _)
                 (is #t)))
        (load-tests _)
        (car _)
        (assoc-ref _ 'test/compound-metadata)))
    (is (equal? '((slow? . #t))
                compound-metadata)))

  (test ("runner merges compound metadata from nested suites and test" ctx)
    (define compound-metadata
      (chain (suite-loader "outer"
               'metadata
               '((shared . outer)
                 (outer? . #t))
               (suite "inner"
                 'metadata
                 '((shared . inner)
                   (inner? . #t))
                 (test ("t1" _)
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

  (test ("suite emits proper values to the test runner" ctx)
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

  (test ("suite-loader creates named suite loader" _)
    (define tmp-suite-loader
      (suite-loader "tmp suite loader" #t))
    (is (suite-loader? tmp-suite-loader))
    (is (not (suite-loader? (lambda () #t)))))

  (test ("define-suite creates suite loader with parenthesized syntax" _)
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

  (test ("define-suite warns for deprecated bare-name syntax" _)
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

(define-suite (documentation-tests)
  (test ("exception, when macro used in place of predicate" _)
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
