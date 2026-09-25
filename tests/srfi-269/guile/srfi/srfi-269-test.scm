;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (srfi srfi-269-test)
  #:use-module ((srfi srfi-269) #:prefix t:)
  #:use-module (test-suite lib))

;; These tests were ported from the (suitbl definitions) test suite.



(define (alist-ref alist key)
  (let ((entry (assq key alist)))
    (and entry (cdr entry))))

(define (alist-contains-key? alist key)
  (and (assq key alist) #t))

(define (all? predicate values)
  (or (null? values)
      (and (predicate (car values))
           (all? predicate (cdr values)))))

(define (make-logging-runner)
  (let ((events '()))
    (lambda (message)
      (if (eq? 'runner/get-log (alist-ref message 'type))
          (reverse events)
          (set! events (cons message events))))))

(define (runner-events thunk)
  (let ((runner (make-logging-runner)))
    (parameterize ((t:current-test-runner runner))
      (thunk)
      (runner '((type . runner/get-log))))))

(define (event-entity event key)
  (alist-ref event key))

(define (event-assertion event)
  (event-entity event 'assertion))

(define (simplify-event event)
  (case (alist-ref event 'type)
    ((runner/load-suite)
     (alist-ref (event-entity event 'suite) 'suite/description))
    ((runner/load-test)
     (alist-ref (event-entity event 'test) 'test/description))
    (else
     (alist-ref (event-assertion event) 'assertion/body))))

(define (eval-suite-definition expression suite-name)
  (let ((module (make-fresh-user-module)))
    (module-use! module (resolve-interface '(srfi srfi-269)))
    (eval expression module)
    (module-ref module suite-name)))



(with-test-prefix "srfi-269"
  (with-test-prefix "test runners"
    (pass-if "set-default-test-runner! installs a runner and returns the previous runner"
      (let ((first-runner (lambda (message) (cons 'first message)))
            (second-runner (lambda (message) (cons 'second message)))
            (original-runner #f))
        (set! original-runner (t:set-default-test-runner! first-runner))
        (dynamic-wind
          (lambda () #t)
          (lambda ()
            (let ((previous-runner
                   (t:set-default-test-runner! second-runner)))
              (and (eq? first-runner previous-runner)
                   (equal? '(second message)
                           ((t:current-test-runner) '(message))))))
          (lambda ()
            (t:set-default-test-runner! original-runner)))))

    (pass-if "parameterized runner takes precedence over changes to the default"
      (let ((default-runner (lambda (_) 'default))
            (updated-runner (lambda (_) 'updated))
            (dynamic-runner (lambda (_) 'dynamic))
            (original-runner #f))
        (set! original-runner (t:set-default-test-runner! default-runner))
        (dynamic-wind
          (lambda () #t)
          (lambda ()
            (let* ((inside-result
                    (parameterize ((t:current-test-runner dynamic-runner))
                      (t:set-default-test-runner! updated-runner)
                      ((t:current-test-runner) 'message)))
                   (outside-result
                    ((t:current-test-runner) 'message)))
              (and (eq? 'dynamic inside-result)
                   (eq? 'updated outside-result))))
          (lambda ()
            (t:set-default-test-runner! original-runner))))))

  (with-test-prefix "predicates"
    (pass-if "test? recognizes only test entities"
      (and (t:test? `((test/body-procedure . ,(lambda (_) #t))
                      (test/description . "test")))
           (not (t:test? '()))
           (not (t:test? `((test/body-procedure . ,(lambda (_) #t)))))
           (not (t:test? '((test/description . "test"))))))

    (pass-if "suite? recognizes only suite entities"
      (and (t:suite? `((suite/body-thunk . ,(lambda () #t))
                       (suite/description . "suite")))
           (not (t:suite? '()))
           (not (t:suite? `((suite/body-thunk . ,(lambda () #t)))))
           (not (t:suite? '((suite/description . "suite"))))))

    (pass-if "suite-loader? identifies suite loaders"
      (let ((loader (t:suite-loader "test suite" #t)))
        (and (t:suite-loader? loader)
             (not (t:suite-loader? (lambda () #t)))))))

  (with-test-prefix "is"
    (pass-if "emits proper values to the test runner"
      (let* ((events
              (runner-events
               (lambda ()
                 (let ((str "a1"))
                   (t:is str)
                   (t:is (= 1 (+ 2 -1)))
                   (t:is str "string assertion")
                   (t:is (= 2 (+ 1 1)) "described assertion")))))
             (assertion-1 (event-assertion (list-ref events 0)))
             (assertion-2 (event-assertion (list-ref events 1)))
             (assertion-3 (event-assertion (list-ref events 2)))
             (assertion-4 (event-assertion (list-ref events 3))))
        (and
         (equal? '(str
                   (= 1 (+ 2 -1))
                   str
                   (= 2 (+ 1 1)))
                 (map simplify-event events))
         (all? (lambda (event)
                 (eq? 'runner/run-assertion (alist-ref event 'type)))
               events)

         (equal? 'str (alist-ref assertion-1 'assertion/body))
         (procedure? (alist-ref assertion-1 'assertion/body-thunk))
         (equal? "a1" ((alist-ref assertion-1 'assertion/body-thunk)))
         (not (alist-contains-key? assertion-1 'assertion/description))

         (equal? '(= 1 (+ 2 -1)) (alist-ref assertion-2 'assertion/body))
         (procedure? (alist-ref assertion-2 'assertion/body-thunk))
         (not (alist-contains-key? assertion-2 'assertion/description))

         (equal? "string assertion" (alist-ref assertion-3 'assertion/description))

         (equal? "described assertion" (alist-ref assertion-4 'assertion/description))))))

  (with-test-prefix "testing"
    (pass-if "captures nested assertion contexts"
      (let* ((events
              (runner-events
               (lambda ()
                 (define (emit-through-procedure)
                   (t:is #t))
                 (t:is #t)
                 (t:testing "outer"
                   (emit-through-procedure)
                   (t:testing "inner"
                     (t:is #t)))
                 (t:is #t))))
             (contexts
              (map (lambda (event)
                     (alist-ref (event-assertion event) 'assertion/context))
                   events)))
        (and (equal? '(()
                       ("outer")
                       ("outer" "inner")
                       ())
                     contexts)
             (= 4 (length events)))))

    (pass-if "evaluates its description once and returns the last body value"
      (let ((evaluations 0))
        (let ((result
               (t:testing (begin (set! evaluations (+ evaluations 1)) "context")
                 'ignored
                 'result)))
          (and (= 1 evaluations)
               (eq? 'result result)))))

    (pass-if "assertion bodies restore context for nested assertions"
      (let* ((outer-event
              (car
               (runner-events
                (lambda ()
                  (t:testing "deferred"
                    (t:is (t:is #t)))))))
             (outer-assertion (event-assertion outer-event))

             (nested-events
              (runner-events
               (lambda ()
                 ((alist-ref outer-assertion 'assertion/body-thunk)))))

             (nested-assertion
              (event-assertion (car nested-events))))
        (and (equal? '("deferred")
                     (alist-ref outer-assertion 'assertion/context))
             (= 1 (length nested-events))
             (equal? '("deferred")
                     (alist-ref nested-assertion 'assertion/context))))))

  (with-test-prefix "test"
    (pass-if "emits proper values to the test runner"
      (let* ((events
              (runner-events
               (lambda ()
                 (t:test "t1" () 'body)
                 (t:test "t2" ()
                   (t:metadata '((good? . #t)))
                   'body)
                 (t:test "t3" (context)
                   (alist-ref context 'answer)))))
             (test-1 (event-entity (list-ref events 0) 'test))
             (test-2 (event-entity (list-ref events 1) 'test))
             (test-3 (event-entity (list-ref events 2) 'test)))
        (and (equal? '("t1" "t2" "t3")
                     (map simplify-event events))
             (all? (lambda (event)
                     (and (eq? 'runner/load-test
                               (alist-ref event 'type))
                          (equal? '()
                                  (alist-ref event
                                             'load/metadata))))
                   events)

             (procedure? (alist-ref test-1 'test/body-procedure))
             (equal? 'body ((alist-ref test-1 'test/body-procedure) '()))
             (eq? #t
                  (alist-ref
                   (alist-ref test-2 'test/metadata)
                   'good?))
             (equal? 'value
                     ((alist-ref test-3 'test/body-procedure)
                      '((answer . value)))))))

    (pass-if "test-loader defers loading and emits call metadata separately"
      (let ((loader #f)
            (construction-events #f))
        (set! construction-events
              (runner-events
               (lambda ()
                 (set! loader
                       (t:test-loader "deferred test" ()
                         (t:metadata
                          '((default? . #t)
                            (shared . default)))
                         #t)))))
        (let* ((default-event (car (runner-events (lambda () (loader)))))
               (amended-event (car
                               (runner-events
                                (lambda ()
                                  (loader '((added? . #t)
                                            (shared . amended)))))))
               (reloaded-event (car (runner-events (lambda () (loader)))))
               (default-entity (event-entity default-event 'test))
               (amended-entity (event-entity amended-event 'test))
               (reloaded-entity (event-entity reloaded-event 'test)))

          (and (null? construction-events)
               (procedure? loader)
               (eq? 'runner/load-test (alist-ref default-event 'type))
               (equal? '() (alist-ref default-event 'load/metadata))

               (equal? "deferred test"
                       (alist-ref default-entity
                                  'test/description))
               (equal? '((default? . #t)
                         (shared . default))
                       (alist-ref default-entity 'test/metadata))
               (t:test? default-entity)
               (equal? '((added? . #t)
                         (shared . amended))
                       (alist-ref amended-event 'load/metadata))
               (equal? '((default? . #t)
                         (shared . default))
                       (alist-ref amended-entity 'test/metadata))
               (equal? '((default? . #t)
                         (shared . default))
                       (alist-ref reloaded-entity
                                  'test/metadata)))))))

  (with-test-prefix "suite"
    (pass-if "emits proper values to the test runner"
      (let* ((events
              (runner-events
               (lambda ()
                 (t:suite "s1" 'body)
                 (t:suite "s2"
                   (t:metadata '((tags . (integration))))
                   'body))))
             (suite-1 (event-entity (list-ref events 0) 'suite))
             (suite-2 (event-entity (list-ref events 1) 'suite)))
        (and (equal? '("s1" "s2")
                     (map simplify-event events))
             (all? (lambda (event)
                     (and (eq? 'runner/load-suite
                               (alist-ref event 'type))
                          (equal? '()
                                  (alist-ref event
                                             'load/metadata))))
                   events)

             (t:suite? suite-1)
             (procedure? (alist-ref suite-1 'suite/body-thunk))
             (equal? 'body ((alist-ref suite-1 'suite/body-thunk)))
             (equal? '(integration)
                     (alist-ref
                      (alist-ref suite-2 'suite/metadata)
                      'tags)))))

    (pass-if "suite-loader defers loading and emits call metadata separately"
      (let ((loader #f)
            (construction-events #f))
        (set! construction-events
              (runner-events
               (lambda ()
                 (set! loader
                       (t:suite-loader "deferred suite"
                         (t:metadata
                          '((default? . #t)
                            (shared . default)))
                         #t)))))
        (let* ((default-event
                (car (runner-events (lambda () (loader)))))
               (amended-event
                (car
                 (runner-events
                  (lambda ()
                    (loader '((added? . #t)
                              (shared . amended)))))))
               (reloaded-event
                (car (runner-events (lambda () (loader)))))
               (default-entity
                (event-entity default-event 'suite))
               (amended-entity
                (event-entity amended-event 'suite))
               (reloaded-entity
                (event-entity reloaded-event 'suite)))
          (and (null? construction-events)
               (procedure? loader)
               (t:suite-loader? loader)
               (not (t:suite-loader? (lambda () #t)))

               (eq? 'runner/load-suite (alist-ref default-event 'type))
               (equal? '() (alist-ref default-event 'load/metadata))
               (equal? "deferred suite"
                       (alist-ref default-entity 'suite/description))
               (equal? '((default? . #t)
                         (shared . default))
                       (alist-ref default-entity 'suite/metadata))
               (t:suite? default-entity)
               (equal? '((added? . #t)
                         (shared . amended))
                       (alist-ref amended-event 'load/metadata))
               (equal? '((default? . #t)
                         (shared . default))
                       (alist-ref amended-entity 'suite/metadata))
               (equal? '((default? . #t)
                         (shared . default))
                       (alist-ref reloaded-entity
                                  'suite/metadata)))))))

  (with-test-prefix "define-suite"
    (pass-if "creates a suite loader with a generated description"
      (let* ((loader
              (eval-suite-definition
               '(define-suite (generated-suite) #t)
               'generated-suite))
             (events (runner-events (lambda () (loader))))
             (suite-entity
              (event-entity (car events) 'suite)))
        (and (t:suite-loader? loader)
             (equal? "generated-suite"
                     (alist-ref suite-entity
                                'suite/description)))))))
