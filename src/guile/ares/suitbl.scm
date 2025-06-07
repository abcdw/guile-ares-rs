;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl)
  #:use-module ((ice-9 atomic)
                #:select (make-atomic-box atomic-box-ref atomic-box-set!))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((ice-9 exceptions) #:select (make-exception-with-message))
  #:use-module ((ice-9 ftw) #:select (nftw))
  #:use-module ((ice-9 regex) #:select (string-match))
  #:use-module ((ares atomic) #:select (atomic-box-update!))
  #:use-module ((srfi srfi-1)
                #:select (last drop-right any fold alist-delete alist-cons))
  #:use-module ((srfi srfi-197) #:select (chain))

  #:export (test-suite test is

            test-reporter-output-port*
            test-reporter-silent
            test-reporter-logging
            test-reporter-unhandled
            test-reporter-base
            test-reporter-dots
            test-reporter-dots-with-hierarchy
            test-reporters-use-all
            test-reporters-use-first

            make-suitbl-test-runner
            test-runner*
            run-test-suites

            ;; TODO: [Andrew Tropin, 2025-05-15] Remove it?, because it
            ;; introduces ambiguity and doesn't have a private
            ;; counterpart
            define-test-suite

            throws-exception?))


#|

SUITBL is Scheme Universal Interactive Testing Base Library

The primary tool of the test library is assert macro @code{is}.  You
can use it on it's own and evaluate it, test runner will take care of
executing the body of assert and calling reporter to report result.

The unevaluated form and thunk returning evaluated arguments are
preserved, so it's up to the test report to provide a nice readable
message.  For example if reporter encounters a failure and sees lset=
as a predicate it can say what the difference between sets instead of
using generic message.

Asserts can be grouped with test macro to make a simple unit of
testing with additional documentation string attached to it.  Test can
also be evaluated on it's own, test runner knows what to do with it.

Tests can be grouped into test suites.  Test suites can be nested in
each other.  It allows to combine related tests, build hierarchy,
control the test execution logic: skipping, shuffling, running in
parallel or whatever.

Test suite is a function, it can be executed to load tests defined
inside.  The name of such functions should contain -tests prefix, it's
not a requirement, but a convention to make it easier for the
developer to visually distinguish functions containing tests
inside (aka test suits) from usual functions.

(define addition-tests
  (test-suite "addition"
    (test "small numbers addition"
      (is (= 4 (+ 2 2)))
      (is (= 7 (+ 3 4))))

    (test "big numbers addition"
      (is (=    4000000000000
             (+ 2000000000000
                2000000000000))))))

(define subtraction-tests
  (test-suite "substraction"
    (test "small numbers subtraction"
      (is (= 1 (- 4 3)))
      (is (= 3 (- 7 4))))))

(define-public arithmetic-tests
  (test-suite "arithmetic"
    (addition-tests)
    (subtraction-tests)))

When you call a test suite, the test runner will build hierarchy of
nested tests and test suites, add it into test runner, later those
loaded tests will be executed.  The order and concurrency of execution
depends on the test runner implementation.

|#


;;;
;;; Primary API
;;;

(define test-runner* (make-parameter #f))

(define (test? x)
  (and (procedure? x)
       (procedure-property x 'suitbl-test?)))

(define (test-suite? x)
  (and (procedure? x)
       (procedure-property x 'suitbl-test-suite?)))

(define (load-test-suite-thunk? x)
  (and (procedure? x)
       (procedure-property x 'suitbl-load-test-suite-thunk?)))

;; We use syntax-rules because it save patterns into transformer's
;; metadata, which allows to generate "signature" of the macro.

(define-syntax is
  (syntax-rules ()
    "A flexible assert macro.  Can be customized by test runner and
reporter."
    ((_ (pred args ...))
     ((test-runner*)
      `((type . run-assert)
        (assert/thunk . ,(lambda () (pred args ...)))
        (assert/arguments-thunk . ,(lambda () (list args ...)))
        (assert/quoted-form . (pred args ...)))))
    ((_ form)
     ((test-runner*)
      `((type . run-assert)
        (assert/thunk . ,(lambda () form))
        (assert/quoted-form . form))))))

(define (alist-merge l1 l2)
  (append l1 l2))

(define-syntax test
  (syntax-rules ()
    "Test represent a logical unit of testing, usually includes zero or
more @code{is} asserts."
    ((test test-description #:metadata metadata expression expressions ...)
     (let ((test-thunk
            (lambda () expression expressions ...)))
       (set-procedure-properties!
        test-thunk
        (alist-merge
         metadata
         `((name . test)
           (documentation . ,test-description)
           (suitbl-test? . #t))))
       ((test-runner*)
        `((type . load-test)
          (test-thunk . ,test-thunk)
          (test-body . (expression expressions ...))))))

    ((test test-description expression expressions ...)
     (test test-description #:metadata '() expression expressions ...))))

(define-syntax test-suite
  (syntax-rules ()
    "Test suite is a grouping unit, it can be executed in parallel,
allows to combine tests and other test suites."
    ((_ suite-description #:metadata metadata expression expressions ...)
     (let* ((load-test-suite-thunk
             (lambda () expression expressions ...))
            (test-suite-thunk
             ;; Wrapping into identity to prevent setting procedure-name
             (identity
              (lambda ()
                ((test-runner*)
                 `((type . load-test-suite)
                   (load-test-suite-thunk . ,load-test-suite-thunk)))))))

       ;; Inside test runner we don't have access to test-suites
       ;; themselves, only to load-test-suite-thunk.
       (set-procedure-properties!
        load-test-suite-thunk
        (alist-merge
         metadata
         `((documentation . ,suite-description)
           ;; We need it to make it possible to customize
           ;; running/skipping logic
           (suitbl-load-test-suite-thunk? . #t))))

       (set-procedure-properties!
        test-suite-thunk
        `((documentation . ,suite-description)
          (load-test-suite . ,load-test-suite-thunk)
          ;; We need it to automate the loading of suites.
          (suitbl-test-suite? . #t)))

       test-suite-thunk))

    ((test-suite suite-description expression expressions ...)
     (test-suite suite-description #:metadata '() expression expressions ...))))

(define-syntax define-test-suite
  (syntax-rules ()
    "Equivalent of (define-public NAME (test-suite ...))."
    ((_ test-suite-name expression ...)
     (begin
       (define-public test-suite-name
         (test-suite (symbol->string 'test-suite-name) expression ...))
       ;; TODO: [Andrew Tropin, 2025-06-02] Maybe remove next line, we
       ;; can set the same value throught test-suite macro
       (set-procedure-property! test-suite-name 'name 'test-suite-name)))))

(define-syntax throws-exception?
  (lambda (x)
    (syntax-case x ()
      ((throws-exception? expression)
       #'(throws-exception? expression exception?))
      ((throws-exception? expression predicate)
       #'(with-exception-handler
          (lambda (ex) (predicate ex))
          (lambda ()
            expression
            #f)
          #:unwind? #t)))))


;;;
;;; Test Reporters
;;;

#|

Test reporters are simple functions which accept a message in format
of Association List (alist) and produce an output to
test-reporter-output-port*.

(test-reporter
 `((type . test-scheduled)
   (test-path . ("test-suite1" "nested-test-suite"))
   (description . "basic arithmetics")))


Test reporters can be comined with test-reporters-use-all or
test-reporters-use-first to compliment each other or override.

A final test reporter can be attached to test runner.

|#

(define test-reporter-output-port* (make-parameter (current-output-port)))

(define (test-reporters-use-all reporters)
  "Create a reporter, which combines all reporters."
  (lambda (message)
    (for-each (lambda (r) (r message)) reporters)))

(define (test-reporters-use-first reporters)
  "Create a reporter, which uses the first successful reporter."
  (lambda (message)
    (let loop ((reporters reporters))
      (unless (null? reporters)
        (let ((reporter-result ((car reporters) message)))
          (or reporter-result (loop (cdr reporters))))))))

(define (test-reporter-silent message)
  "Do nothing, return @code{#t}."
  #t)

(define (test-reporter-logging message)
  "Just log the @code{message}."
  (format (test-reporter-output-port*) "message: ~y" message))

(define (test-reporter-unhandled message)
  "A simple test reporter, which prints incomming message.  It can be
combined with another reporter using @code{test-reporters-use-first}
to catch unhandled messages."
  (format (test-reporter-output-port*)
          "\nmessage is not handled:\n~y\n" message))

(define (string-repeat s n)
  "Returns string S repeated N times."
  (fold
   (lambda (_ str)
     (string-append str s))
   ""
   (iota n)))

(define (tests->pretty-string l)
  (map
   (lambda (i)
     (cond
      ((test? i) (string-append "test: " (procedure-documentation i)))
      ((load-test-suite-thunk? i)
       (string-append "suite: " (procedure-documentation i)))
      ((list? i) (tests->pretty-string i))
      (else i)))
   l))

(define (test-reporter-hierarchy message)
  (case (assoc-ref message 'type)
    ((test-scheduled)
     (format (test-reporter-output-port*) "~a"
             (string-repeat "|" (length (assoc-ref message 'test-path))))
     (format (test-reporter-output-port*) " + test ~a\n"
             (assoc-ref message 'description)))
    ((test-suite-enter)
     (format (test-reporter-output-port*) "~a"
             (string-append
              (string-repeat "|" (length (assoc-ref message 'test-path)))
              "┌"))
     (format (test-reporter-output-port*) "> ~a\n"
             (assoc-ref message 'description)))
    ((test-suite-leave)
     (format (test-reporter-output-port*) "~a"
             (string-append
              (string-repeat "|" (length (assoc-ref message 'test-path)))
              "└"))
     (format (test-reporter-output-port*) "> ~a\n"
             (assoc-ref message 'description)))
    ((print-test-suite)
     (format (test-reporter-output-port*) "~y"
             (tests->pretty-string (assoc-ref message 'tset-suite))))
    (else #f)))


(define (safify-thunk thunk)
  (lambda ()
    (with-exception-handler
     (lambda (ex)
       `(exception . ,ex))
     (lambda ()
       `(value . ,(thunk)))
     #:unwind? #t)))

(define (test-reporter-verbose message)
  (define (actual message)
    (let* ((quoted-form (assoc-ref message 'assert/quoted-form))
           (arguments-thunk (assoc-ref message 'assert/arguments-thunk))
           (safe-arguments-thunk (safify-thunk arguments-thunk)))
      ;; TODO: [Andrew Tropin, 2025-05-28] Ensure arguments-thunk
      ;; exception handled.
      (if (and (list? quoted-form) (= 3 (length quoted-form)))
          (match (safe-arguments-thunk)
            ((value . (first second))
             (format #f "~a and ~a are not ~a" first second (car quoted-form)))
            ((exception . ex)
             (format #f "Evaluation of arguments thunk failed with:\n~a" ex)))
          (assoc-ref message 'assert/result))))

  (case (assoc-ref message 'type)
    ((test-start)
     (format (test-reporter-output-port*) "\n┌Test ~a\n"
             (assoc-ref message 'description)))
    ((test-end)
     (format (test-reporter-output-port*) "└Test ~a\n"
             (assoc-ref message 'description)))

    ((assert-pass)
     (format (test-reporter-output-port*) "~y✓\n"
             (assoc-ref message 'assert/quoted-form)))

    ((assert-fail)
     (format (test-reporter-output-port*) "~y✗ ~a\n"
             (assoc-ref message 'assert/quoted-form) (actual message)))

    ((assert-error)
     (format (test-reporter-output-port*) "~y✗ produced error:\n ~s\n"
             (assoc-ref message 'assert/quoted-form)
             (assoc-ref message 'assert/error)))

    (else #f)))

(define test-reporter-base
  (chain (list test-reporter-verbose test-reporter-hierarchy)
    (test-reporters-use-all _)
    (list _ test-reporter-unhandled)
    (test-reporters-use-first _)))

(define (test-reporter-dots message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type
    ((test-suite-start)
     (format (test-reporter-output-port*) "["))
    ((test-suite-end)
     (format (test-reporter-output-port*) "]"))

    ((test-start)
     (format (test-reporter-output-port*) "("))
    ((test-end)
     (format (test-reporter-output-port*) ")"))
    ((test-skip)
     (format (test-reporter-output-port*) "(S)"))

    ((assert-pass)
     (format (test-reporter-output-port*) "."))
    ((assert-fail)
     (format (test-reporter-output-port*) "F"))
    ((assert-error)
     (format (test-reporter-output-port*) "E"))

    (else #f)))

(define test-reporter-dots-with-hierarchy
  (chain (list test-reporter-dots test-reporter-hierarchy)
    (test-reporters-use-all _)
    (list _ test-reporter-unhandled)
    (test-reporters-use-first _)))

(define-syntax simple-profile
  (lambda (stx)
    (syntax-case stx ()
      ((_ expressions ...)
       #'(let ((start-time (get-internal-real-time))
               (return-value expressions ...))
           (format (test-reporter-output-port*) "run time: ~f\n"
                   (exact->inexact
                    (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
           return-value)))))


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

(define (update-atomic-alist-value! alist-atom key f)
  (atomic-box-update!
   alist-atom
   (lambda (alist)
     (let* ((value (or (assoc-ref alist key) #f))
            (new-value (f value)))
       (chain alist
         (alist-delete key _)
         (alist-cons key new-value _))))))

(define* (make-suitbl-test-runner
          #:key
          (test-reporter test-reporter-base))
  "A flexible test runner factory, which spawns new test runners."
  ;; TODO: [Andrew Tropin, 2025-06-05] Get rid of dynamic variables,
  ;; they can cause problems when using with continuations and thus
  ;; with concurrent test runs implemented on top of fibers
  (define %test-path* (make-parameter '()))
  (define %test* (make-parameter #f))
  (define %test-events* (make-parameter #f))
  (define %current-test-suite-items* (make-parameter #f))
  (define %schedule-only?* (make-parameter #f))

  ;; TODO: [Andrew Tropin, 2025-06-05] Combine state into one variable
  ;; and make it accessible via "class" methods.
  (define state (make-atomic-box '()))
  (define last-run-summary (make-atomic-box #f))
  (define this #f)
  (define reporter-state (make-atomic-box '()))

  (define %test-reporter
    (lambda (message)
      (test-reporter
       (alist-cons 'state reporter-state message))))

  (define (%run-test test-thunk)
    (parameterize ((%test-events* (make-atomic-box '())))
      ;; TODO: [Andrew Tropin, 2025-04-24] Handle exceptions that can
      ;; happen inside test case, but outside of assert

      ;; What to do with exception outside of assert?
      (test-thunk)
      (atomic-box-ref (%test-events*))))

  (define (run-test test)
    (let* ((result (%run-test test))
           (error? (any (lambda (x) (eq? x 'error)) result))
           (fail? (any (lambda (x) (eq? x 'fail)) result)))
      `((errors . ,(if error? 1 0))
        (failures . ,(if (and fail? (not error?)) 1 0))
        (assertions . ,(length result))
        (tests . 1))))

  (define initial-run-summary
    `((errors . 0)
      (failures . 0)
      (assertions . 0)
      (tests . 0)))

  (define (run-test-suite suite)
    (let ((result #f))
      (%test-reporter
       `((type . test-suite-start)
         (description . ,(car suite))))
      (set! result
            (let loop ((summary initial-run-summary)
                       (remaining-items (cdr suite)))
              (if (null? remaining-items)
                  summary
                  (let ((item (car remaining-items)))
                    (loop
                     (merge-run-summaries
                      summary
                      ((if (test? item) run-test run-test-suite)
                       item))
                     (cdr remaining-items))))))
      (%test-reporter
       `((type . test-suite-end)
         (description . ,(car suite))))
      result))

  (define (default-run-assert form-thunk args-thunk quoted-form)
    (with-exception-handler
     (lambda (ex)
       (when (%test*)
         (atomic-box-update!
          (%test-events*)
          (lambda (value)
            (cons 'error value))))
       (%test-reporter
        `((type . assert-error)
          (assert/quoted-form . ,quoted-form)
          (assert/error . ,ex))))
     (lambda ()
       ;; TODO: [Andrew Tropin, 2024-12-23] Write down evaluation time
       ;; TODO: [Andrew Tropin, 2024-12-23] Report start before evaling the form
       (let* ((result (form-thunk)))
         (when (%test*)
           (atomic-box-update!
            (%test-events*)
            (lambda (value)
              (cons (if result 'pass 'fail) value))))
         (%test-reporter
          `((type . ,(if result 'assert-pass 'assert-fail))
            (assert/result . ,result)
            (assert/arguments-thunk . ,args-thunk)
            (assert/quoted-form . ,quoted-form)))
         result))
     #:unwind? #t))

  (define (print-test-suite suite)
    (%test-reporter
     `((type . print-test-suite)
       (test-suite . ,suite))))

  (define (make-try-load-suite load-test-suite-thunk)
    (define description
      (procedure-documentation load-test-suite-thunk))

    (define test-suite-enter!
      (lambda ()
        (%test-reporter
         `((type . test-suite-enter)
           (test-path . ,(%test-path*))
           (description . ,description)))))
    (define test-suite-leave!
      (lambda ()
        (%test-reporter
         `((type . test-suite-leave)
           (test-path . ,(%test-path*))
           (description . ,description)))))

    (lambda ()
      (test-suite-enter!)
      (define result
        (with-exception-handler
         (lambda (ex)
           (cons 'exception ex))
         (lambda ()
           (when (%test*)
             (chain "Test Suite can't be nested into Test Macro"
               (make-exception-with-message _)
               (raise-exception _)))
           (parameterize ((%current-test-suite-items* (make-atomic-box '()))
                          (%test-path* (cons description (%test-path*))))
             (load-test-suite-thunk)
             (chain (%current-test-suite-items*)
               (atomic-box-ref _)
               (reverse _)
               (cons load-test-suite-thunk _)
               (cons 'value _))))
         #:unwind? #t))
      (test-suite-leave!)
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

      ((load-test-suite)
       (let* ((try-load-suite (make-try-load-suite
                               (assoc-ref x 'load-test-suite-thunk))))

         (match (try-load-suite)
           (('exception . ex)
            (raise-exception ex))
           (('value . val)
            (let ((suite-items (%current-test-suite-items*)))
              (if suite-items
                  (atomic-box-update!
                   suite-items
                   (lambda (items) (cons val items)))

                  (update-atomic-alist-value! state 'suite (lambda (l) val))))
            val))
         ;; test-runner-run-test-suites sets %schedule-only?*
         ;; and also calls run-scheduled-tests, so to prevent double
         ;; execution of scheduled test suites we add this condition.
         (when (and (null? (%test-path*)) (not (%schedule-only?*)))
           (this `((type . run-scheduled-tests))))))

      ((load-test)
       (let* ((original-test-thunk (assoc-ref x 'test-thunk))
              (description (procedure-documentation original-test-thunk))
              (new-test-thunk
               (lambda ()
                 (when (%test*)
                   (chain "Test Macros can't be nested"
                          (make-exception-with-message _)
                          (raise-exception _)))
                 (%test-reporter
                  `((type . test-start)
                    (description . ,description)))
                 (parameterize ((%test* description))
                   (original-test-thunk))
                 (%test-reporter
                  `((type . test-end)
                    (description . ,description))))))

         (copy-procedure-properties! original-test-thunk new-test-thunk)

         (let ((suite-items (%current-test-suite-items*)))
           (if suite-items
               (begin
                 (atomic-box-update!
                  suite-items
                  (lambda (items) (cons new-test-thunk items)))
                 (%test-reporter
                  `((type . test-scheduled)
                    (test-path . ,(%test-path*))
                    (description . ,description))))
               (begin
                 (atomic-box-set!
                  last-run-summary
                  (run-test new-test-thunk)))))

         *unspecified*))

      ((run-assert)
       (let ((assert-thunk (assoc-ref x 'assert/thunk))
             (assert-arguments-thunk (assoc-ref x 'assert/arguments-thunk))
             (assert-quoted-form (assoc-ref x 'assert/quoted-form)))
         (when (and (not (null? (%test-path*)))
                    (not (%test*)))
           (chain
            "Assert encountered inside test-suite, but outside of test"
            (make-exception-with-message _)
            (raise-exception _)))
         (default-run-assert
           assert-thunk assert-arguments-thunk assert-quoted-form)))

      ((run-scheduled-tests)
       ;; (print-test-suite (assoc-ref (atomic-box-ref state) 'suite))
       (atomic-box-set!
        last-run-summary
        (chain
         (atomic-box-ref state)
         (assoc-ref _ 'suite)
         (run-test-suite _)))

       (atomic-box-ref last-run-summary))

      ((get-run-summary)
       (atomic-box-ref last-run-summary))

      ((run-test-suites)
       (parameterize ((test-runner* this)
                      (%schedule-only?* #t))
         ;; TODO: [Andrew Tropin, 2025-05-01] Call reset-runner-state
         (for-each (lambda (ts) (ts)) (assoc-ref x 'test-suites))

         (test-runner
          `((type . run-scheduled-tests)))
         ;; TODO: [Andrew Tropin, 2025-05-01] Call get-last-run-summary
         ))

      (else
       (chain msg-type
         (format #f "no handler for message type ~a" _)
         (make-exception-with-message _)
         (raise-exception _)))))

  (set! this test-runner)
  this)

(define (run-test-suites test-runner test-suites)
  (test-runner
   `((type . run-test-suites)
     (test-suites . ,test-suites))))

;; Set default test runner.
(test-runner* (make-suitbl-test-runner))


;;;
;;; Test Suite Exploration API
;;;

(define (get-test-module name)
  ;; TODO: Handle the case, when test module doesn't exist.
  "Return a test module related to the specified.  Usually it's a module with
-test prefix.  Return current module if it already contains -test prefix."
  (let* ((m-name name)
         (m-tail (last m-name))
         (test-m-tail
          (if (string-suffix? "-test" (symbol->string m-tail))
              m-tail
              (symbol-append m-tail '-test))))
    (resolve-module
     (append
      (drop-right m-name 1)
      (list test-m-tail)))))

;; (get-test-module '(ares suitbl))

(define (get-module-test-suites module)
  (filter
   identity
   (module-map (lambda (k v)
                 (and (variable-bound? v)
                      (test-suite? (variable-ref v))
                      (variable-ref v)))
               module)))

(define (get-module-public-test-suites module)
  (get-module-test-suites (module-public-interface module)))

(define private-samples-tests
  (test-suite "sample private test suite"
    (test "sample test"
      (is "true assert"))))
;; (private-samples-tests)

(define* (load-test-modules-thunk
          #:key
          (test-file-pattern ".*-test(\\.scm|\\.ss)")
          (load-file (lambda (p rp)
                       (format #t "loading test module: ~a\n" p)
                       (primitive-load-path rp))))
  "Return a thunk, which loads all the modules matching TEST-FILE-PATTERN
using LOAD-FILE procedure, which accepts path and relative to %load-path path."
  (lambda ()
    (for-each
     (lambda (path)
       (nftw
        path
        (lambda (file-path _ flags _1 _2)
          (when (eq? flags 'regular)
            (define relative-path
              (string-drop file-path (1+ (string-length path))))
            (when (string-match test-file-pattern file-path)
              (load-file file-path relative-path)))
          #t)))
     %load-path)))

(define* (get-all-test-modules
          #:key
          (load-project-test-modules? #t)
          (load-all-test-modules (load-test-modules-thunk)))
  (when load-project-test-modules?
    (load-all-test-modules))
  ;; TODO: [Andrew Tropin, 2025-05-09] Use module-name regex instead
  ;; of suffix match?
  (filter (lambda (m)
            (string-suffix? "-test" (symbol->string (last (module-name m)))))
          ((@ (ares reflection modules) all-modules))))

;; ((@ (ice-9 pretty-print) pretty-print)
;;  (map
;;   (lambda (m)
;;     (cons
;;      (module-name m)
;;      (get-module-public-test-suites m))) (get-all-test-modules)))

;; (define-test-suite public-suite
;;   'hey)

;; (variable-set!)
;; (get-module-test-suites (module-public-interface (current-module)))
