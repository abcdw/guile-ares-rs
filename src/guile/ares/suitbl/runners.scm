;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl runners)
  #:use-module ((ares atomic) #:select (atomic-box-update!))
  #:use-module ((ares suitbl definitions) #:select (test-runner* test?))
  #:use-module ((ares suitbl reporters) #:select (test-reporter-base))

  #:use-module ((ice-9 atomic)
                #:select (make-atomic-box atomic-box-ref atomic-box-set!))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((ice-9 exceptions) #:select (make-exception-with-message))

  #:use-module ((srfi srfi-1)
                #:select (last drop-right any fold alist-delete alist-cons))
  #:use-module ((srfi srfi-197) #:select (chain chain-and))

  #:use-module ((ares suitbl runner-state) #:prefix state:)
  #:export (make-suitbl-test-runner
            run-test-suites

            get-stats
            get-loaded-tests

            set-runner-config-value!))


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
;;; Helpers
;;;

(define (copy-procedure-properties! from to)
  (set-procedure-properties! to (procedure-properties from)))


;;;
;;; Test runner
;;;

(define* (make-suitbl-test-runner
          #:key
          (config '())
          (default-config `((auto-run? . #t)
                            (test-reporter . ,test-reporter-base)
                            (reset-loaded-tests-on-suite-load? . #t)
                            (log-runner-messages? . #f))))
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
  (define %test* (make-parameter #f))
  (define %test-run-events* (make-parameter #f))
  (define %test-reporter* (make-parameter
                           (state:get-runner-config-value
                            state 'test-reporter)))

  (define (get-test-reporter)
    (lambda (message)
      ((%test-reporter*)
       ;; (chain message
       ;;   (alist-cons 'state reporter-state _)
       ;;   (alist-cons 'test-runner this _))
       message)))

  (define (%run-assert assert)
    (let ((body-thunk (assoc-ref assert 'assert/body-thunk)))
      (with-exception-handler
       (lambda (ex)
         (when (%test*)
           (atomic-box-update!
            (%test-run-events*)
            (lambda (value)
              (cons 'error value))))
         ((get-test-reporter)
          (append
           `((type . reporter/assertion-error)
             (assertion/error . ,ex))
           assert)))
       (lambda ()
         ;; TODO: [Andrew Tropin, 2024-12-23] Write down evaluation time
         ;; TODO: [Andrew Tropin, 2024-12-23] Report start before evaling the form
         (let* ((result (body-thunk)))
           (when (%test*)
             (atomic-box-update!
              (%test-run-events*)
              (lambda (value)
                (cons (if result 'pass 'fail) value))))
           ((get-test-reporter)
            (append
             `((type . ,(if result
                            'reporter/assertion-pass
                            'reporter/assertion-fail))
               (assertion/result . ,result))
             assert))
           result))
       #:unwind? #t)))

  (define (run-assert ctx)
    (let* ((assert (chain ctx
                     (get-message _)
                     (assoc-ref _ 'assert))))
      (when (and (not (null? (%suite-path*)))
                 (not (%test*)))
        (chain
            "Assert encountered inside suite, but outside of test"
          (make-exception-with-message _)
          (raise-exception _)))
      (%run-assert assert)))

  (define (%run-test test)
    ;; TODO: [Andrew Tropin, 2025-04-24] Handle exceptions that can
    ;; happen inside test case, but outside of assert

    ;; What to do with exception outside of assert?

    (let ((description (assoc-ref test 'test/description))
          (test-body-thunk (assoc-ref test 'test/body-thunk)))
      (when (%test*)
        (chain "Test Macros can't be nested"
          (make-exception-with-message _)
          (raise-exception _)))
      ((get-test-reporter)
       `((type . reporter/test-start)
         (description . ,description)))
      ;; TODO: [Andrew Tropin, 2025-08-02] Change %test* to
      ;; %inside-test?*
      (define result
        (parameterize ((%test* description)
                       (%test-run-events* (make-atomic-box '())))
          (test-body-thunk)
          (atomic-box-ref (%test-run-events*))))

      ((get-test-reporter)
       `((type . reporter/test-end)
         (description . ,description)))

      result))

  (define (run-test test)
    (let* ((result (%run-test test))
           (error? (any (lambda (x) (eq? x 'error)) result))
           (fail? (any (lambda (x) (eq? x 'fail)) result)))
      `((test . ,test)
        (test-run/result . ((tests . 1)
                            (failures . ,(if (and fail? (not error?)) 1 0))
                            (errors . ,(if error? 1 0))
                            (skipped . 0)
                            (assertions . ,(length result)))))))

  (define (print-suite suite)
    ((get-test-reporter)
     `((type . reporter/print-suite)
       (show-suite-info . ,procedure-documentation)
       (suite . ,suite))))

  (define (make-try-load-suite suite)
    (define suite-body-thunk
      (assoc-ref suite 'suite/body-thunk))
    (define description
      (assoc-ref suite 'suite/description))

    (define suite-enter!
      (lambda ()
        ((get-test-reporter)
         `((type . reporter/suite-enter)
           (suite-path . ,(%suite-path*))
           (description . ,description)))))
    (define suite-leave!
      (lambda ()
        ((get-test-reporter)
         `((type . reporter/suite-leave)
           (suite-path . ,(%suite-path*))
           (description . ,description)))))

    (lambda ()
      (define (make-suite-node suite children)
        `((suite . ,suite)
          (suite/children . ,children)))
      (suite-enter!)
      (define result
        (with-exception-handler
         (lambda (ex)
           (cons 'exception ex))
         (lambda ()
           (when (%test*)
             (chain "Test Suite can't be nested into Test Macro"
               (make-exception-with-message _)
               (raise-exception _)))
           (parameterize ((%current-suite-node-items* (make-atomic-box '()))
                          (%suite-path* (cons suite (%suite-path*))))
             (suite-body-thunk)
             (chain (%current-suite-node-items*)
               (atomic-box-ref _)
               (reverse _)
               (make-suite-node suite _)
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
         (assoc-ref _ 'runner/state)
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
        (runner/state . ,state)))

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

                  (state:add-suite-tree! state val)))
            val))
         (when (and (null? (%suite-path*))
                    (state:get-runner-config-value state 'auto-run?))
           (this `((type . runner/run-tests))))))

      ((runner/run-tests)
       (let* ((runner-config (get-runner-cfg ctx))
              (reporter (assoc-ref runner-config 'test-reporter))
              (test-execution-results
               (if reporter
                   (parameterize ((%test-reporter* reporter))
                     (map run-test
                          (state:get-scheduled-tests state runner-config)))
                   (map run-test
                        (state:get-scheduled-tests state runner-config)))))

         (state:save-run-history! state test-execution-results))

       *unspecified*)

      ((runner/load-test)
       (let* ((test (chain ctx
                      (get-message _)
                      (assoc-ref _ 'test)))
              (test-with-context
               (cons `(suite/path . ,(reverse (%suite-path*))) test))
              (description (assoc-ref test 'test/description)))

         (state:add-loaded-test! state test-with-context)

         ((get-test-reporter)
          `((type . reporter/test-loaded)
            (suite-path . ,(%suite-path*))
            (description . ,description)))

         (let ((suite-node-items (%current-suite-node-items*)))
           ;; (pk (%suite-path*))
           (if suite-node-items
               (atomic-box-update!
                suite-node-items
                (lambda (items) (cons test-with-context items)))
               (when (state:get-runner-config-value state 'auto-run?)
                 (this `((type . runner/run-tests))))))

         *unspecified*))

      ((runner/get-run-history)
       (state:get-run-history state))

      ((runner/get-run-summary)
       (state:get-run-summary state))

      (else
       (chain msg-type
         (format #f "no handler for message type ~a" _)
         (make-exception-with-message _)
         (raise-exception _)))))

  (set! this test-runner)
  this)

;; Set default test runner.
(test-runner* (make-suitbl-test-runner))
