;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl runners)
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
                #:select (last drop-right fold alist-delete alist-cons))
  #:use-module ((srfi srfi-197) #:select (chain chain-and chain-when))

  #:use-module ((ares suitbl state) #:prefix state:)
  #:use-module ((ares suitbl running) #:prefix running:)
  #:export (make-suitbl-test-runner
            make-silent-test-runner))


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

(define* (make-suitbl-test-runner
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
  (define %assertion-executions* (make-parameter #f))
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

  (define (%run-assert assert inside-test? assertion-executions)
    (let* ((body-thunk (assoc-ref assert 'assert/body-thunk))
           ;; TODO: [Andrew Tropin, 2024-12-23] Write down evaluation time
           (run-result (running:with-exception-continuation body-thunk))
           (assertion-execution
            (running:make-assertion-execution assert run-result))
           (reporter-message
            (running:assertion-execution->reporter-message
             assertion-execution)))

      (when inside-test?
        (atomic-box-update!
         assertion-executions
         (lambda (value)
           (cons assertion-execution value))))

      ((get-test-reporter)
       reporter-message)

      ;; We re-raise it here, inside run-assert to make it work for
      ;; lonely (is ...)  evaluation case
      (if (and (re-raise?) (running:raised? run-result))
          ((running:raised-continuation run-result))
          (if (running:returned? run-result)
              (running:returned-value run-result)
              *unspecified*))))

  (define (run-assert ctx)
    (let* ((assert (chain ctx
                     (get-message _)
                     (assoc-ref _ 'assert)))
           (inside-test? (%inside-test?*))
           (assertion-executions (%assertion-executions*)))
      (when (and (not (null? (%suite-path*)))
                 (not inside-test?))
        (raise-suitbl-wrong-position-exception
         'is 'suite-body
         "Assert encountered inside suite, but outside of test"))
      (%run-assert assert inside-test? assertion-executions)))

  (define (%run-test test)
    (let ((description (assoc-ref test 'test/description))
          (test-body-thunk (assoc-ref test 'test/body-thunk)))
      (when (%inside-test?*)
        (raise-suitbl-wrong-position-exception
         'test 'test-body
         "Test Macros can't be nested"))
      ((get-test-reporter)
       `((type . run/test-start)
         (description . ,description)))

      (define result
        (parameterize ((%inside-test?* #t)
                       (%assertion-executions* (make-atomic-box '())))
          (let ((run-result
                 (running:with-exception-continuation test-body-thunk)))
            (define assertion-executions
              (reverse (atomic-box-ref (%assertion-executions*))))

            ((get-test-reporter)
             `((type . run/test-end)
               (description . ,description)))

            (when (running:raised? run-result)
              (if (re-raise?)
                  ((running:raised-continuation run-result))
                  (raise-exception
                   (running:raised-exception run-result))))

            assertion-executions)))

      result))

  (define (run-test test)
    "Test can either pass, fail or error.

test-run/summary carries assertion counters, while test-run/outcome
carries the final verdict."
    (let* ((assertion-executions (%run-test test))
           (assertion-summary
            (running:assertion-executions->assertion-summary
             assertion-executions))
           (test-run-summary
            (running:assertion-summary->test-run-summary
             assertion-summary))
           (test-run-outcome
            (running:run-summary->run-outcome
             test-run-summary)))
      `((test . ,test)
        (test-run/assertions . ,assertion-executions)
        (test-run/summary . ,test-run-summary)
        (test-run/outcome . ,test-run-outcome))))

  (define (make-try-load-suite suite)
    (define suite-body-thunk
      (assoc-ref suite 'suite/body-thunk))
    (define description
      (assoc-ref suite 'suite/description))

    (define suite-enter!
      (lambda ()
        ((get-test-reporter)
         `((type . load/suite-enter)
           (suite-path . ,(%suite-path*))
           (description . ,description)))))
    (define suite-leave!
      (lambda ()
        ((get-test-reporter)
         `((type . load/suite-leave)
           (suite-path . ,(%suite-path*))
           (description . ,description)))))

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
                 ((get-test-reporter)
                  `((type . run/start)))
                 (let ((test-execution-results
                        (map run-test
                             (state:get-scheduled-tests state runner-config))))
                   (state:save-run-history! state test-execution-results)
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
              (test-with-context
               (cons `(suite/path . ,(reverse (%suite-path*))) test))
              (description (assoc-ref test 'test/description)))

         (state:add-loaded-test! state test-with-context)

         ((get-test-reporter)
          `((type . load/test)
            (suite-path . ,(%suite-path*))
            (description . ,description)))

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
(test-runner* (make-suitbl-test-runner))

(define (make-silent-test-runner)
  (make-suitbl-test-runner
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
