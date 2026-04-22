;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl runner)
  #:use-module ((ares atomic) #:select (atomic-box-update!))
  #:use-module ((ares suitbl definitions) #:select (test-runner* test?))
  #:use-module ((ares suitbl exceptions)
                #:select (raise-suitbl-wrong-position-exception))
  #:use-module ((ares suitbl reporters) #:prefix reporter:)

  #:use-module ((ice-9 atomic)
                #:select (make-atomic-box atomic-box-ref atomic-box-set!))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((ice-9 exceptions) #:select (make-exception-with-message))

  #:use-module ((srfi srfi-1)
                #:select (last drop-right fold alist-delete alist-cons find iota))
  #:use-module ((srfi srfi-197) #:select (chain chain-and chain-when))

  #:use-module ((ares suitbl state) #:prefix state:)
  #:use-module ((ares suitbl running) #:prefix running:)
  #:export (get-state
            make-suitbl
            make-silent))


;;;
;;; Test Runners
;;;

#|

The same test runner can be used to run and re-run tests and test
suits multiple times.

There is a test-runner* dynamic variable (parameter), to reset test
environment just set it to new instance of test runner.

|#


;;;
;;; Test runner
;;;

(define* (make-suitbl
          #:key
          (config '())
          (default-config `((auto-run? . #t)
                            (test-reporter . ,reporter:base)
                            (reporting/port . ,(current-output-port))
                            (reset-loaded-tests-on-suite-load? . #t)
                            (log-runner-messages? . #f)
                            (re-raise? . #f))))
  "A flexible test runner factory, which spawns new test runners."
  (define state
    (make-atomic-box `((runner/run-history . #f)
                       (runner/config . ,(state:merge-runner-config
                                          config
                                          default-config)))))
  (define this #f)

  ;; TODO: [Andrew Tropin, 2025-06-05] Get rid of dynamic variables,
  ;; they can cause problems when using with continuations and thus
  ;; with concurrent test runs implemented on top of fibers
  (define %suite-path* (make-parameter '()))
  (define %current-suite-node-items* (make-parameter #f))
  (define %inside-test?* (make-parameter #f))
  (define %assertion-runs* (make-parameter #f))
  (define %test-reporter* (make-parameter
                           (state:get-runner-config-value
                            state 'test-reporter)))

  (define (get-test-reporter)
    (lambda (message)
      (let ((port (state:get-runner-config-value state 'reporting/port)))
        (chain-when message
          (port (acons 'reporting/port port _))
          (#t   (acons 'suitbl/state state _))
          (#t   ((%test-reporter*) _))))))

  (define (re-raise?)
    (state:get-runner-config-value state 're-raise?))

  (define (first-erroring-assertion-run assertion-runs)
    (find (lambda (assertion-run)
            (running:raised?
             (assoc-ref assertion-run 'assertion-run/result)))
          assertion-runs))

  (define (suite-path->metadata suite-path)
    (if (null? suite-path)
        '()
        (append
         (or (assoc-ref (car suite-path) 'suite/metadata) '())
         (suite-path->metadata (cdr suite-path)))))

  (define (compound-test-metadata test suites)
    (append
     (or (assoc-ref test 'test/metadata) '())
     (suite-path->metadata suites)))

  (define (%run-assert assertion inside-test? assertion-runs)
    (let* ((body-thunk (assoc-ref assertion 'assertion/body-thunk))
           ;; TODO: [Andrew Tropin, 2024-12-23] Write down evaluation time
           (run-result (running:with-exception-continuation body-thunk))
           (assertion-run
            (running:make-assertion-run assertion run-result))
           (reporter-message
            (running:assertion-run->reporter-message
             assertion-run)))

      (when inside-test?
        (atomic-box-update!
         assertion-runs
         (lambda (value)
           (cons assertion-run value))))

      ((get-test-reporter)
       reporter-message)

      ;; Re-raise lonely (is ...) immediately, but defer assertion
      ;; replays executed inside tests until %run-test finishes.
      (if (and (re-raise?)
               (running:raised? run-result)
               (not inside-test?))
          ((running:raised-continuation run-result))
          (if (running:returned? run-result)
              (running:returned-value run-result)
              *unspecified*))))

  (define (run-assert ctx)
    (let* ((assertion (chain ctx
                         (get-message _)
                         (assoc-ref _ 'assertion)))
           (inside-test? (%inside-test?*))
           (assertion-runs (%assertion-runs*)))
      (when (and (not (null? (%suite-path*)))
                 (not inside-test?))
        (raise-suitbl-wrong-position-exception
         'is 'suite-body
         "Assert encountered inside suite, but outside of test"))
      (%run-assert assertion inside-test? assertion-runs)))

  (define* (%run-test test #:key run-progress)
    (let ((test-body-thunk (assoc-ref test 'test/body-thunk)))
      (when (%inside-test?*)
        (raise-suitbl-wrong-position-exception
         'test 'test-body
         "Test Macros can't be nested"))
      ((get-test-reporter)
       `((type . run/test-start)
         (test . ,test)
         ,@(if run-progress
               `((run-progress . ,run-progress))
               '())))

      (define result
        (parameterize ((%inside-test?* #t)
                       (%assertion-runs* (make-atomic-box '())))
          (let ((test-run-result
                 (running:with-exception-continuation test-body-thunk)))
            (define assertion-runs
              (reverse (atomic-box-ref (%assertion-runs*))))
            (define test-run
              (running:make-test-run test test-run-result assertion-runs))

            ((get-test-reporter)
             `((type . run/test-end)
               (test . ,test)
               (test-run . ,test-run)
               ,@(if run-progress
                     `((run-progress . ,run-progress))
                     '())))

            (when (re-raise?)

              (define raised-assertion-run
                (first-erroring-assertion-run
                 assertion-runs))
              (when raised-assertion-run
                ((running:raised-continuation
                  (assoc-ref raised-assertion-run
                             'assertion-run/result))))

              (when (running:raised? test-run-result)
                ((running:raised-continuation test-run-result))))

            test-run)))

      result))

  (define* (run-test test #:key run-progress)
    "Test can either pass, fail or error.

test-run/summary carries assertion counters, while test-run/outcome
carries the final verdict."
    (%run-test test #:run-progress run-progress))

  (define (make-try-load-suite suite)
    (define suite-body-thunk
      (assoc-ref suite 'suite/body-thunk))

    (define suite-enter!
      (lambda ()
        ((get-test-reporter)
         `((type . load/suite-enter)
           (suite-path . ,(%suite-path*))
           (suite . ,suite)))))
    (define suite-leave!
      (lambda ()
        ((get-test-reporter)
         `((type . load/suite-leave)
           (suite-path . ,(%suite-path*))
           (suite . ,suite)))))

    (lambda ()
      (suite-enter!)
      (define result
        (with-exception-handler
         (lambda (ex)
           (cons 'exception ex))
         (lambda ()
           (when (%inside-test?*)
             (raise-suitbl-wrong-position-exception
              'suite 'test-body
              "Test Suite can't be nested into Test Macro"))
           (parameterize ((%current-suite-node-items* (make-atomic-box '()))
                          (%suite-path* (cons suite (%suite-path*))))
             (suite-body-thunk)
             (chain (%current-suite-node-items*)
               (atomic-box-ref _)
               (reverse _)
               (state:make-suite-node suite _)
               (cons 'value _))))
         #:unwind? #t))
      (suite-leave!)
      result))

  (define (get-runner-cfg ctx)
    (define message-cfg
      (or
       (chain-and ctx
         (assoc-ref _ 'runner/message)
         (assoc-ref _ 'runner/config))
       '()))

    (define state-cfg
      (or
       (chain-and ctx
         (assoc-ref _ 'suitbl/state)
         (atomic-box-ref _)
         (assoc-ref _ 'runner/config))
       '()))

    (state:merge-runner-config message-cfg state-cfg))

  (define (get-message ctx)
    (assoc-ref ctx 'runner/message))

  (define (message-type ctx)
    (chain ctx
      (get-message _)
      (assoc-ref _ 'type)))

  (define (logging? ctx)
    (and
     (assoc-ref
      (get-runner-cfg ctx)
      'log-runner-messages?)
     (not (member (message-type ctx)
                  '(runner/get-state runner/get-log)))))

  (define (test-runner message)
    "Default test runner"

    (define ctx
      `((runner/message . ,message)
        (suitbl/state . ,state)))

    (when (logging? ctx)
      (state:save-event! state (get-message ctx)))

    (define msg-type (message-type ctx))

    (case msg-type
      ((runner/get-state)
       state)
      ((runner/get-log)
       (state:get-log state))

      ((runner/run-assert)
       (run-assert ctx))

      ((runner/run-tests)
       (let* ((runner-config (get-runner-cfg ctx))
              (reporter (assoc-ref runner-config 'test-reporter))
              (run-tests!
               (lambda ()
                 (let* ((scheduled-tests
                         (state:get-scheduled-tests state runner-config))
                        (loaded-tests (state:get-loaded-tests state))
                        (run-plan
                         `((plan/scheduled-count . ,(length scheduled-tests))
                           (plan/loaded-count . ,(length loaded-tests)))))
                   ((get-test-reporter)
                    `((type . run/start)
                      (run-plan . ,run-plan)))

                   (let* ((total (length scheduled-tests))
                          (run-history
                           (map (lambda (test current)
                                  (run-test test
                                            #:run-progress
                                            `((progress/current . ,current)
                                              (progress/total . ,total))))
                                scheduled-tests
                                (iota total 1))))
                     (state:save-run-history! state run-history))

                   ((get-test-reporter)
                      `((type . run/end)))))))
         (if reporter
             (parameterize ((%test-reporter* reporter))
               (run-tests!))
             (run-tests!)))

       *unspecified*)

      ((runner/load-test)
       (when (and (null? (%suite-path*))
                  (state:get-runner-config-value
                   state 'reset-loaded-tests-on-suite-load?))
         (state:reset-loaded-tests! state))
       (let* ((test (chain ctx
                      (get-message _)
                      (assoc-ref _ 'test)))
              (suite-path (reverse (%suite-path*)))
              (compound-metadata
               (compound-test-metadata test (%suite-path*)))
              (test-with-context
               (chain test
                 (alist-cons 'test/compound-metadata compound-metadata _)
                 (alist-cons 'suite/path suite-path _))))

         (state:add-loaded-test! state test-with-context)

         ((get-test-reporter)
          `((type . load/test)
            (suite-path . ,(%suite-path*))
            (test . ,test-with-context)))

         (let ((suite-node-items (%current-suite-node-items*)))
           ;; (pk (%suite-path*))
           (if suite-node-items
               (atomic-box-update!
                suite-node-items
                (lambda (items) (cons
                                 (state:make-test-node test-with-context)
                                 items)))
               (when (state:get-runner-config-value state 'auto-run?)
                 (this `((type . runner/run-tests))))))

         *unspecified*))

      ((runner/load-suite)
       (when (and (null? (%suite-path*))
                  (state:get-runner-config-value
                   state 'reset-loaded-tests-on-suite-load?))
         (state:reset-loaded-tests! state))
       (let* ((suite (chain ctx
                       (get-message _)
                       (assoc-ref _ 'suite)))
              (try-load-suite (make-try-load-suite suite)))

         (match (try-load-suite)
           (('exception . ex)
            (raise-exception ex))
           (('value . val)
            (let ((suite-node-items (%current-suite-node-items*)))
              (if suite-node-items
                  (atomic-box-update!
                   suite-node-items
                   (lambda (items) (cons val items)))

                  (begin
                    (state:add-suite-tree! state val)
                    ((get-test-reporter)
                     `((type . load/end)
                       (suite-node . ,val))))))
            val))
         (when (and (null? (%suite-path*))
                    (state:get-runner-config-value state 'auto-run?))
           (this `((type . runner/run-tests))))))

      (else
       (chain msg-type
         (format #f "no handler for message type ~a" _)
         (make-exception-with-message _)
         (raise-exception _)))))

  (set! this test-runner)
  this)

;; Set default test runner.
(test-runner* (make-suitbl))

(define* (get-state #:optional (runner (test-runner*)))
  (runner '((type . runner/get-state))))

(define (make-silent)
  (make-suitbl
   #:config `((test-reporter . ,reporter:silent))))


(define-syntax simple-profile
  (lambda (stx)
    (syntax-case stx ()
      ((_ expressions ...)
       #'(let ((start-time (get-internal-real-time))
               (return-value expressions ...))
           (format (current-error-port) "run time: ~f\n"
                   (exact->inexact
                    (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
           return-value)))))
