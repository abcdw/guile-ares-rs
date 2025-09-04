;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl runners)
  #:use-module ((ares atomic) #:select (atomic-box-update!))
  #:use-module ((ares suitbl core) #:select (test-runner* test?))
  #:use-module ((ares suitbl reporters) #:select (test-reporter-base))

  #:use-module ((ice-9 atomic)
                #:select (make-atomic-box atomic-box-ref atomic-box-set!))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((ice-9 exceptions) #:select (make-exception-with-message))
  #:use-module ((ice-9 match) #:select (match))

  #:use-module ((srfi srfi-1)
                #:select (last drop-right any fold alist-delete alist-cons))
  #:use-module ((srfi srfi-197) #:select (chain chain-and))

  #:export (make-suitbl-test-runner
            run-test-suites

            get-stats

            set-run-config-value!))


;;;
;;; Test Runners
;;;

#|

The same test runner can be used to run and re-run tests and test
suits multiple times.

There is a test-runner* dynamic variable (parameter), to reset test
environment just set it to new instance of test runner.

|#

(define (merge-run-summaries s1 s2)
  (map
   (lambda (v)
     (match v
       ((key . value)
        (cons key (+ (assoc-ref s2 key) value)))))
   s1))

(define (copy-procedure-properties! from to)
  (set-procedure-properties! to (procedure-properties from)))

(define (update-alist-value alist key value)
  (chain alist
    (alist-delete key _)
    (alist-cons key value _)))

(define (update-atomic-alist-value! alist-atom key f)
  (atomic-box-update!
   alist-atom
   (lambda (alist)
     (let* ((value (or (assoc-ref alist key) #f))
            (new-value (f value)))
       (update-alist-value alist key new-value)))))


;;;
;;; Test runner
;;;

(define* (make-suitbl-test-runner
          #:key
          (test-reporter test-reporter-base)
          (run-config '((auto-run? . #t)
                        (reset-loaded-tests-on-suite-load? . #t))))
  "A flexible test runner factory, which spawns new test runners."
  ;; TODO: [Andrew Tropin, 2025-06-05] Get rid of dynamic variables,
  ;; they can cause problems when using with continuations and thus
  ;; with concurrent test runs implemented on top of fibers
  (define %suite-path* (make-parameter '()))
  (define %test* (make-parameter #f))
  (define %test-events* (make-parameter #f))
  (define %current-suite-items* (make-parameter #f))
  (define %schedule-only?* (make-parameter #f))
  (define %run-config* (make-parameter #f))

  ;; TODO: [Andrew Tropin, 2025-06-05] Combine state into one variable
  ;; and make it accessible via "class" methods.
  (define state (make-atomic-box `((run-config . ,run-config))))
  (define last-run-summary (make-atomic-box #f))
  (define this #f)
  (define reporter-state (make-atomic-box '()))

  (define initial-run-summary
    `((errors . 0)
      (failures . 0)
      (assertions . 0)
      (tests . 0)))

  (define %test-reporter
    (lambda (message)
      (test-reporter
       ;; (chain message
       ;;   (alist-cons 'state reporter-state _)
       ;;   (alist-cons 'test-runner this _))
       message)))

  (define (%run-test test)
    (parameterize ((%test-events* (make-atomic-box '())))
      ;; TODO: [Andrew Tropin, 2025-04-24] Handle exceptions that can
      ;; happen inside test case, but outside of assert

      ;; What to do with exception outside of assert?

      (let ((description (assoc-ref test 'test/description))
            (test-body-thunk (assoc-ref test 'test/body-thunk)))
        (when (%test*)
          (chain "Test Macros can't be nested"
            (make-exception-with-message _)
            (raise-exception _)))
        (%test-reporter
         `((type . test-start)
           (description . ,description)))
        ;; TODO: [Andrew Tropin, 2025-08-02] Change %test* to
        ;; %inside-test?*
        (parameterize ((%test* description))
          (test-body-thunk))
        (%test-reporter
         `((type . test-end)
           (description . ,description))))

      (atomic-box-ref (%test-events*))))

  (define (run-test test)
    (let* ((result (%run-test test))
           (error? (any (lambda (x) (eq? x 'error)) result))
           (fail? (any (lambda (x) (eq? x 'fail)) result)))
      `((errors . ,(if error? 1 0))
        (failures . ,(if (and fail? (not error?)) 1 0))
        (assertions . ,(length result))
        (tests . 1))))

  (define (run-assert assert)
    (let ((body-thunk (assoc-ref assert 'assert/body-thunk)))
      (with-exception-handler
       (lambda (ex)
         (when (%test*)
           (atomic-box-update!
            (%test-events*)
            (lambda (value)
              (cons 'error value))))
         (%test-reporter
          (append
           `((type . assertion-error)
             (assertion/error . ,ex))
           assert)))
       (lambda ()
         ;; TODO: [Andrew Tropin, 2024-12-23] Write down evaluation time
         ;; TODO: [Andrew Tropin, 2024-12-23] Report start before evaling the form
         (let* ((result (body-thunk)))
           (when (%test*)
             (atomic-box-update!
              (%test-events*)
              (lambda (value)
                (cons (if result 'pass 'fail) value))))
           (%test-reporter
            (append
             `((type . ,(if result 'assertion-pass 'assertion-fail))
               (assertion/result . ,result))
             assert))
           result))
       #:unwind? #t)))

  (define (print-suite suite)
    (%test-reporter
     `((type . print-suite)
       (show-suite-info . ,procedure-documentation)
       (suite . ,suite))))

  (define (make-try-load-suite suite)
    (define suite-body-thunk
      (assoc-ref suite 'suite/body-thunk))
    (define description
      (assoc-ref suite 'suite/description))

    (define suite-enter!
      (lambda ()
        (%test-reporter
         `((type . suite-enter)
           (suite-path . ,(%suite-path*))
           (description . ,description)))))
    (define suite-leave!
      (lambda ()
        (%test-reporter
         `((type . suite-leave)
           (suite-path . ,(%suite-path*))
           (description . ,description)))))

    (lambda ()
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
           (parameterize ((%current-suite-items* (make-atomic-box '()))
                          (%suite-path* (cons suite-body-thunk (%suite-path*))))
             (suite-body-thunk)
             (chain (%current-suite-items*)
               (atomic-box-ref _)
               (reverse _)
               (cons suite-body-thunk _)
               (cons 'value _))))
         #:unwind? #t))
      (suite-leave!)
      result))

  (define (test-runner x)
    "Default test runner"
    (unless (member (assoc-ref x 'type) '(get-state get-log))
      (update-atomic-alist-value!
       state 'events
       (lambda (l)
         (cons x (or l '())))))

    (define msg-type (assoc-ref x 'type))

    (case msg-type
      ((get-state)
       state)
      ((get-log)
       (reverse (or (assoc-ref (atomic-box-ref state) 'events) '())))

      ((run-assert)
       (let* ((assert (assoc-ref x 'assert)))
         (when (and (not (null? (%suite-path*)))
                    (not (%test*)))
           (chain
               "Assert encountered inside suite, but outside of test"
             (make-exception-with-message _)
             (raise-exception _)))
         (run-assert assert)))

      ((load-suite)
       (when (and (null? (%suite-path*))
                  (get-run-config-value
                   state 'reset-loaded-tests-on-suite-load?))
         (reset-loaded-tests! state))
       (let* ((suite (assoc-ref x 'suite))
              (try-load-suite (make-try-load-suite suite)))

         (match (try-load-suite)
           (('exception . ex)
            (raise-exception ex))
           (('value . val)
            (let ((suite-items (%current-suite-items*)))
              (if suite-items
                  (atomic-box-update!
                   suite-items
                   (lambda (items) (cons val items)))

                  (update-atomic-alist-value! state 'suite (lambda (l) val))))
            val))
         ;; test-runner-run-suites sets %schedule-only?*
         ;; and also calls run-scheduled-tests, so to prevent double
         ;; execution of scheduled test suites we add this condition.
         (when (and (null? (%suite-path*)) (not (%schedule-only?*))
                    (get-run-config-value state 'auto-run?))
           (this `((type . run-tests))))))

      ((run-tests)
       (atomic-box-set!
        last-run-summary
        (let* ((run-config (or (assoc-ref x 'run-config)
                               (get-run-config state)))
               (test-execution-results
                (map run-test (get-scheduled-tests state run-config))))
          (let loop ((summary initial-run-summary)
                     (remaining-items test-execution-results))
            (if (null? remaining-items)
                summary
                (let ((item (car remaining-items)))
                  (loop
                   (merge-run-summaries summary item)
                   (cdr remaining-items)))))))

       ;; TODO: [Andrew Tropin, 2025-09-04] Temporary return run
       ;; summary for easier implementation of test runner integration
       ;; into IDE, remove it, when proper integration is implemented
       (atomic-box-ref last-run-summary))

      ((run-suites)
       (parameterize ((test-runner* this)
                      (%schedule-only?* #t))
         ;; TODO: [Andrew Tropin, 2025-05-01] Call reset-runner-state
         (for-each
          (lambda (ts) (ts))
          (assoc-ref x 'suites))

         (test-runner
          `((type . run-tests)))
         ;; TODO: [Andrew Tropin, 2025-05-01] Call get-last-run-summary
         ))

      ((load-test)
       (let* ((test (assoc-ref x 'test))
              (test-with-context (cons `(suite/path . ,%suite-path*) test))
              (description (assoc-ref test 'test/description)))

         (add-loaded-test! state test-with-context)

         (%test-reporter
          `((type . test-loaded)
            (suite-path . ,(%suite-path*))
            (description . ,description)))

         (let ((suite-items (%current-suite-items*)))
           ;; (pk (%suite-path*))
           (if suite-items
               (atomic-box-update!
                suite-items
                (lambda (items) (cons test items)))
               (when (get-run-config-value state 'auto-run?)
                 (atomic-box-set!
                  last-run-summary
                  (run-test test)))))

         *unspecified*))

      ((get-run-summary)
       (atomic-box-ref last-run-summary))

      (else
       (chain msg-type
         (format #f "no handler for message type ~a" _)
         (make-exception-with-message _)
         (raise-exception _)))))

  (set! this test-runner)
  this)

(define (run-test-suites test-runner suites)
  (test-runner
   `((type . run-suites)
     (suites . ,suites))))

;; Set default test runner.
(test-runner* (make-suitbl-test-runner))


;;;
;;; Tests state management
;;;

;; It should be after test-runner* is set (which is kinda strange)

(define (get-loaded-tests state)
  (chain (atomic-box-ref state)
    (assoc-ref _ 'loaded-tests)
    (or _ '())))

(define (add-loaded-test! state test)
  (update-atomic-alist-value!
   state 'loaded-tests
   (lambda (l) (cons test (or l '())))))

(define (reset-loaded-tests! state)
  (update-atomic-alist-value!
   state 'loaded-tests
   (lambda (l) '())))

(define (get-scheduled-tests state run-config)
  (let ((lot-transformation identity))
    (chain (atomic-box-ref state)
      (assoc-ref _ 'loaded-tests)
      (or _ '())
      (lot-transformation _))))

(define (get-run-config state)
  (chain (atomic-box-ref state)
    (assoc-ref _ 'run-config)
    (or _ '())))

(define (get-run-config-value state key)
  (chain-and (atomic-box-ref state)
    (assoc-ref _ 'run-config)
    (assoc-ref _ key)))

(define (set-run-config-value! state key value)
  (update-atomic-alist-value!
   state 'run-config
   (lambda (alist) (update-alist-value (or alist '()) key value))))

(define (get-stats state)
  (let* ((state-val (atomic-box-ref state))
         (loaded-tests-count (chain state
                               (get-loaded-tests _)
                               (length _))))
    `((loaded-tests-count . ,loaded-tests-count)
      (selected-tests-count . ,loaded-tests-count))))
