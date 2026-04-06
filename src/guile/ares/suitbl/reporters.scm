;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl reporters)
  #:use-module ((ares suitbl definitions) #:select (test? suite?))
  #:use-module ((ares suitbl state) #:prefix state:)
  #:use-module ((ares guile exceptions) #:select (exception->string))
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((srfi srfi-197) #:select (chain))

  #:use-module ((ice-9 exceptions) #:select (make-exception-with-message))
  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((sxml simple) #:select (sxml->xml))

  #:export (test-reporter-output-port*
            test-reporter-silent
            test-reporter-logging
            test-reporter-unhandled
            test-reporter-base
            test-reporter-dots
            test-reporter-dots-with-hierarchy
            test-reporter-minimal
            test-reporter-spying
            test-reporters-use-all
            test-reporters-use-first

            forest->junit-sxml
            forest->junit-xml
            test-reporter-junit

            suite-forest->tree-string
            count-suites-and-tests
            test-reporter-tree
            test-reporter-loaded-summary
            test-reporter-run-summary))



;;;
;;; Test Reporters
;;;

#|

Test reporters are simple functions which accept a message in format
of Association List (alist) and produce an output to
test-reporter-output-port*.

(test-reporter
 `((type . reporter/test-loaded)
   (suite-path . ("suite1" "nested-suite"))
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
      ((test? i) (string-append "test: " (assoc-ref i 'test/description)))
      ((suite? i)
       (string-append "suite: " (assoc-ref i 'suite/description)))
      ((list? i) (tests->pretty-string i))
      (else i)))
   l))

(define (test-reporter-hierarchy message)
  (case (assoc-ref message 'type)
    ((reporter/test-loaded)
     (format (test-reporter-output-port*) "~a"
             (string-repeat "|" (length (assoc-ref message 'suite-path))))
     (format (test-reporter-output-port*) " + test ~a\n"
             (assoc-ref message 'description)))
    ((reporter/suite-enter)
     (format (test-reporter-output-port*) "~a"
             (string-append
              (string-repeat "|" (length (assoc-ref message 'suite-path)))
              "┌"))
     (format (test-reporter-output-port*) "> ~a\n"
             (assoc-ref message 'description)))
    ((reporter/suite-leave)
     (format (test-reporter-output-port*) "~a"
             (string-append
              (string-repeat "|" (length (assoc-ref message 'suite-path)))
              "└"))
     (format (test-reporter-output-port*) "> ~a\n"
             (assoc-ref message 'description)))

    ((reporter/print-suite)
     (format (test-reporter-output-port*) "~y"
             (tests->pretty-string (assoc-ref message 'suite))))
    (else #f)))

(define flaky-throw
  (let ((throw? #f))
    (lambda ()
      (if throw?
          (begin (set! throw? #f)
                 (raise-exception
                  (make-exception-with-message
                   "flaky exception")))
          (begin (set! throw? #t) 'ho)))))

;; (is (eq? 'hi (flaky-throw)))

;; (is (eq? 'hey 'ho))

;; (is 78)

(define (format-location message)
  "Extract assert/location from MESSAGE and format it as a human-readable
string like \"file:line:column\".  Line numbers are converted to 1-indexed.
Returns an empty string if no location is available."
  (let ((location (assoc-ref message 'assert/location)))
    (if (and location (list? location))
        (let ((filename (assoc-ref location 'filename))
              (line (assoc-ref location 'line))
              (column (assoc-ref location 'column)))
          (format #f "~a:~a:~a"
                  (or filename "<unknown>")
                  (if line (1+ line) "?")
                  (or column "?")))
        "")))

(define (safify-thunk thunk)
  (lambda ()
    (with-exception-handler
     (lambda (ex)
       `(exception . ,ex))
     (lambda ()
       `(value . ,(thunk)))
     #:unwind? #t)))

(define (pretty-string obj)
  (format #f "~y" obj))

(define (actual message)
    (let* ((assert-body (assoc-ref message 'assert/body))
           (args-thunk (assoc-ref message 'assert/args-thunk))
           (safe-args-thunk (safify-thunk args-thunk)))
      (if (and (list? assert-body) (= 3 (length assert-body)))
          (match (safe-args-thunk)
            ((value . (first second))
             (format #f "\n~a and\n~a are not ~a"
                     (pretty-string first)
                     (pretty-string second)
                     (car assert-body)))
            ((exception . ex)
             (format #f "Evaluation of arguments thunk failed with:\n~a" ex)))
          (assoc-ref message 'assertion/result))))

(define (test-reporter-verbose message)
  (case (assoc-ref message 'type)
    ((reporter/test-start)
     (format (test-reporter-output-port*) "\n┌Test ~a\n"
             (assoc-ref message 'description)))
    ((reporter/test-end)
     (format (test-reporter-output-port*) "└Test ~a\n"
             (assoc-ref message 'description)))

    ((reporter/assertion-pass)
     (format (test-reporter-output-port*) "~y✓\n"
             (assoc-ref message 'assert/body)))

    ((reporter/assertion-fail)
     (format (test-reporter-output-port*) "~a\n~y✗ ~a\n"
             (format-location message)
             (assoc-ref message 'assert/body) (actual message)))

    ((reporter/assertion-error)
     (format (test-reporter-output-port*) "~a\n~y✗ produced error:\n ~s\n"
             (format-location message)
             (assoc-ref message 'assert/body)
             (exception->string
              (assoc-ref message 'assertion/error))))

    (else #f)))

(define (test-reporter-loading-minimal message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type
    ((reporter/test-loaded)
     (format (test-reporter-output-port*) "-> ~a\n"
             (assoc-ref message 'description)))

    ;; ((reporter/suite-enter)
    ;;  (format (test-reporter-output-port*) "["))
    ;; ((reporter/suite-leave)
    ;;  (format (test-reporter-output-port*) "]"))
    (else #f)))

(define (test-reporter-execution-minimal message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type

    ((reporter/test-start)
     (format (test-reporter-output-port*) "--- [~a] ---\n"
             (assoc-ref message 'description)))
    ((reporter/test-end)
     (format (test-reporter-output-port*) "\n"))

    ((reporter/assertion-pass)
     (format (test-reporter-output-port*) "✓"))

    ((reporter/assertion-fail)
     (format (test-reporter-output-port*) "~a\n~y✗ ~a\n"
             (format-location message)
             (assoc-ref message 'assert/body) (actual message)))

    ((reporter/assertion-error)
     (format (test-reporter-output-port*) "~a\n~y✗ produced error:\n ~s\n"
             (format-location message)
             (assoc-ref message 'assert/body)
             (exception->string
              (assoc-ref message 'assertion/error))))

    (else #f)))

(define test-reporter-minimal
  (chain (list
          test-reporter-execution-minimal
          test-reporter-loading-minimal)
    (test-reporters-use-all _)
    (list _ test-reporter-unhandled)
    (test-reporters-use-first _)))

(define (pre-evaled-expression message)
    (let* ((assert-body (assoc-ref message 'assert/body))
           (args-thunk (assoc-ref message 'assert/args-thunk))
           (safe-args-thunk (safify-thunk args-thunk)))
      (if (list? assert-body)
          (match (safe-args-thunk)
            ((value . evaluated-args)
             (cons (car assert-body) evaluated-args))
            ((exception . ex)
             (format #f "Evaluation of arguments thunk failed with:\n~a" ex)))
          (assoc-ref message 'assertion/result))))

(define (test-reporter-execution-spying message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type

    ((reporter/test-start)
     (format (test-reporter-output-port*) "--- [~a] ---\n"
             (assoc-ref message 'description)))
    ((reporter/test-end)
     (format (test-reporter-output-port*) "\n"))

    ((reporter/assertion-pass reporter/assertion-fail)
     (format (test-reporter-output-port*) "~y~y => ~y"
             (assoc-ref message 'assert/body)
             (pre-evaled-expression message)
             (assoc-ref message 'assertion/result)))

    ((reporter/assertion-error)
     (format (test-reporter-output-port*) "\n ~a\n ~y✗ produced error:\n ~s\n"
             (format-location message)
             (assoc-ref message 'assert/body)
             (exception->string
              (assoc-ref message 'assertion/error))))

    (else #f)))

(define test-reporter-spying
  (chain (list
          test-reporter-execution-spying
          test-reporter-loading-minimal)
    (test-reporters-use-all _)
    (list _ test-reporter-unhandled)
    (test-reporters-use-first _)))

(define (test-reporter-tree message)
  "A reporter that prints the complete suite tree (like the @code{tree}
CLI command) when a top-level suite finishes loading."
  (case (assoc-ref message 'type)
    ((reporter/suite-tree-loaded)
     (let ((suite-node (assoc-ref message 'suite-node)))
       (format (test-reporter-output-port*) "\n~a"
               (suite-forest->tree-string (list suite-node)))))
    (else #f)))

(define (count-suites-and-tests node)
  "Count suites, tests, module suites, and empty suites in a tree NODE.
Returns an alist with keys: suites, tests, module-suites, empty-suites."
  (define zero-counts
    '((suites . 0) (tests . 0) (module-suites . 0) (empty-suites . 0)))

  (define (merge-counts c1 c2)
    "Sum corresponding values of two count alists."
    (map (lambda (entry)
           (cons (car entry) (+ (cdr entry) (assoc-ref c2 (car entry)))))
         c1))

  (cond
   ((assoc-ref node 'test)
    (acons 'tests 1 zero-counts))
   ((assoc-ref node 'suite)
    (let* ((suite (assoc-ref node 'suite))
           (children (tree-node-children node))
           (metadata (or (and (list? suite)
                              (assoc-ref suite 'suite/metadata))
                         '()))
           (module? (assoc-ref metadata 'module-suite?))
           (empty? (null? children))
           (child-counts (fold merge-counts zero-counts
                               (map count-suites-and-tests children))))
      (merge-counts
       `((suites . 1)
         (tests . 0)
         (module-suites . ,(if module? 1 0))
         (empty-suites . ,(if empty? 1 0)))
       child-counts)))
   (else zero-counts)))

(define (test-reporter-loaded-summary message)
  "A reporter that prints the number of loaded suites and tests
when a top-level suite finishes loading."
  (case (assoc-ref message 'type)
    ((reporter/suite-tree-loaded)
     (let* ((suite-node (assoc-ref message 'suite-node))
            (counts (count-suites-and-tests suite-node))
            (suites (assoc-ref counts 'suites))
            (tests (assoc-ref counts 'tests))
            (modules (assoc-ref counts 'module-suites))
            (empty (assoc-ref counts 'empty-suites)))
       (format (test-reporter-output-port*)
               "Loaded ~a test~p and ~a suite~p (~a module~p, ~a empty).\n"
               tests tests suites suites modules modules empty)))
    (else #f)))

(define (test-reporter-run-summary message)
  "A reporter that prints a summary line after all tests have been executed."
  (case (assoc-ref message 'type)
    ((reporter/run-end)
     (let ((summary (state:get-run-summary (assoc-ref message 'state))))
       (if summary
           (let ((tests (assoc-ref summary 'tests))
                 (assertions (assoc-ref summary 'assertions))
                 (failures (assoc-ref summary 'failures))
                 (errors (assoc-ref summary 'errors)))
             (format (test-reporter-output-port*)
                     "Ran ~a assertion~p in ~a test~p: ~a failure~p, ~a error~p.\n"
                     assertions assertions
                     tests tests
                     failures failures
                     errors errors))
           (format (test-reporter-output-port*)
                   "No test results available.\n"))))
    (else #f)))

(define test-reporter-base
  (chain (list test-reporter-verbose
               ;; test-reporter-hierarchy
               test-reporter-tree
               test-reporter-loaded-summary
               test-reporter-run-summary)
    (test-reporters-use-all _)
    (list _ test-reporter-unhandled)
    (test-reporters-use-first _)))

(define (test-reporter-dots message)
  (define msg-type (assoc-ref message 'type))
  (case msg-type
    ((reporter/suite-start)
     (format (test-reporter-output-port*) "["))
    ((reporter/suite-end)
     (format (test-reporter-output-port*) "]"))

    ((reporter/test-start)
     (format (test-reporter-output-port*) "("))
    ((reporter/test-end)
     (format (test-reporter-output-port*) ")"))
    ((reporter/test-skip)
     (format (test-reporter-output-port*) "(S)"))

    ((reporter/assertion-pass)
     (format (test-reporter-output-port*) "."))
    ((reporter/assertion-fail)
     (format (test-reporter-output-port*) "F"))
    ((reporter/assertion-error)
     (format (test-reporter-output-port*) "E"))

    (else #f)))

(define test-reporter-dots-with-hierarchy
  (chain (list test-reporter-dots test-reporter-hierarchy)
    (test-reporters-use-all _)
    (list _ test-reporter-unhandled)
    (test-reporters-use-first _)))


;;;
;;; JUnit XML
;;;

(define (suite-result->attributes result)
  "Convert suite-run/result alist to JUnit testsuite attributes"
  `((tests ,(number->string (assoc-ref result 'tests)))
    (failures ,(number->string (assoc-ref result 'failures)))
    (errors ,(number->string (assoc-ref result 'errors)))
    (skipped ,(number->string (assoc-ref result 'skipped)))
    (assertions ,(number->string (assoc-ref result 'assertions)))))

(define (test-result->attributes result)
  "Convert test-run/result alist to JUnit testcase attributes"
  `((assertions ,(number->string (assoc-ref result 'assertions)))))

(define (node->junit-sxml node classname-path)
  "Convert a single node (suite or test) to JUnit SXML"
  (cond
   ;; Suite node
   ((and (assoc-ref node 'suite)
         (assoc-ref node 'suite-node/children))
    (let* ((suite (assoc-ref node 'suite))
           (suite-name (assoc-ref suite 'suite/description))
           (children (assoc-ref node 'suite-node/children))
           (suite-result (assoc-ref node 'suite-run/result))
           (new-classname-path
            (if (null? classname-path)
                suite-name
                (string-append classname-path "." suite-name)))
           (attributes (if suite-result
                           (cons `(name ,suite-name)
                                 (suite-result->attributes suite-result))
                           `((name ,suite-name)))))
      `(testsuite (@ ,@attributes)
                  ,@(map (lambda (child)
                           (node->junit-sxml child new-classname-path))
                         children))))

   ;; Test node
   ((assoc-ref node 'test)
    (let* ((test (assoc-ref node 'test))
           (test-name (assoc-ref test 'test/description))
           (test-result (assoc-ref node 'test-run/result))
           (errors (and test-result (assoc-ref test-result 'errors)))
           (failures (and test-result (assoc-ref test-result 'failures)))
           (attributes (append
                        `((name ,test-name)
                          (classname ,classname-path))
                        (if test-result
                            (test-result->attributes test-result)
                            '())))
           (status-element (cond
                            ((and errors (> errors 0))
                             '((error (@ (message "Test had errors")
                                         (type "TestError")))))
                            ((and failures (> failures 0))
                             '((failure (@ (message "Test failed")
                                           (type "AssertionError")))))
                            (else '()))))
      `(testcase (@ ,@attributes)
                 ,@status-element)))

   (else '())))

(define (calculate-totals forest)
  "Calculate total statistics from all top-level suites"
  (fold (lambda (node acc)
          (let ((result (assoc-ref node 'suite-run/result)))
            (if result
                (map (lambda (key-val)
                       (match key-val
                         ((key val)
                          (cons key (+ val (assoc-ref result key))))))
                     acc)
                acc)))
        '((tests 0)
          (failures 0)
          (errors 0)
          (skipped 0)
          (assertions 0))
        forest))

(define* (forest->junit-sxml forest #:key (name "Test run"))
  "Convert a forest-with-summary to JUnit SXML format"
  (let* ((totals (calculate-totals forest))
         (root-attributes (cons `(name ,name)
                                (suite-result->attributes totals))))
    `(*TOP*
      (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
      (testsuites (@ ,@root-attributes)
                  ,@(map (lambda (node)
                          (node->junit-sxml node ""))
                        forest)))))

(define* (forest->junit-xml forest #:key (name "Test run"))
  (with-output-to-string
    (lambda ()
      (sxml->xml (forest->junit-sxml forest #:name name)))))

(define (test-reporter-junit message)
  "A test reporter that emits JUnit XML to @code{test-reporter-output-port*}
after all tests have finished running.  Silent for all other message types."
  (case (assoc-ref message 'type)
    ((reporter/run-end)
     (let* ((state (assoc-ref message 'state))
            (forest (state:get-suite-forest-with-summary state))
            (xml (forest->junit-xml forest)))
       (format (test-reporter-output-port*) "~a\n" xml)))
    (else #f)))

;;;
;;; Tree-style formatting (like the `tree` CLI command)
;;;

(define (tree-node-emoji node)
  "Return an emoji prefix for a tree NODE based on its type and metadata."
  (let ((s (assoc-ref node 'suite))
        (t (assoc-ref node 'test)))
    (cond
     ((and t)
      (let ((metadata (and (list? t)
                           (or (assoc-ref t 'test/metadata) '()))))
        (if (and metadata (assoc-ref metadata 'slow?))
            "📄🐌 "
            "📄 ")))
     ((and s (list? s))
      (let ((metadata (or (assoc-ref s 'suite/metadata) '())))
        (cond
         ((assoc-ref metadata 'project-suite?) "🗄️ ")
         ((assoc-ref metadata 'module-suite?) "🗂️ ")
         (else "📂 "))))
     (else ""))))

(define (tree-node-description node)
  "Get the description string from a tree NODE.
Works with both raw nodes (suite/test are alists) and simplified
nodes (suite/test are strings).  Raw nodes get an emoji prefix based
on their type and metadata."
  (let ((s (assoc-ref node 'suite))
        (t (assoc-ref node 'test))
        (emoji (tree-node-emoji node)))
    (cond
     ((and s (string? s)) s)
     ((and s (list? s))
      (string-append emoji (assoc-ref s 'suite/description)))
     ((and t (string? t)) t)
     ((and t (list? t))
      (string-append emoji (assoc-ref t 'test/description)))
     (else "<unknown>"))))

(define (tree-node-children node)
  "Get the children list of a tree NODE, or @code{'()} for test nodes."
  (or (assoc-ref node 'suite-node/children) '()))

(define (format-tree-children children prefix port)
  "Write CHILDREN nodes to PORT with tree connectors under PREFIX."
  (let loop ((remaining children))
    (unless (null? remaining)
      (let* ((child (car remaining))
             (last? (null? (cdr remaining)))
             (connector (if last? "└─ " "├─ "))
             (extension (if last? "   " "│  "))
             (desc (tree-node-description child))
             (grandchildren (tree-node-children child)))
        (format port "~a~a~a\n" prefix connector desc)
        (format-tree-children
         grandchildren (string-append prefix extension) port)
        (loop (cdr remaining))))))

(define (suite-forest->tree-string forest)
  "Format a suite FOREST as a tree string, similar to the @code{tree}
CLI command output.  Works with both raw and simplified forests.

@example
first suite
├── good one
├── nested-suite
│   └── failing test
└── another good one
@end example"
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (root)
         (let ((desc (tree-node-description root))
               (children (tree-node-children root)))
           (format #t "~a\n" desc)
           (format-tree-children children "" (current-output-port))))
       forest))))
