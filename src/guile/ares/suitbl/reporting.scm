;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2024, 2025, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl reporting)
  #:use-module ((ares suitbl definitions) #:select (test? suite?))
  #:use-module ((ares suitbl state) #:prefix state:)
  #:use-module ((ares suitbl running) #:prefix running:)
  #:use-module ((ares guile exceptions) #:select (exception->string))
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((srfi srfi-197) #:select (chain chain-and))

  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((sxml simple) #:select (sxml->xml))

  #:export (string-repeat
            tests->pretty-string
            format-location
            format-assertion-failure-detail
            pre-evaled-expression
            format-assertion-minimal
            format-assertion-verbose

            forest->junit-sxml
            forest->junit-xml

            format-test-compact
            format-test-twoline
            format-test-verbose

            suite-forest->tree-string
            tree-node-children
            tree-node-description
            count-suites-and-tests))

;;;
;;; String utilities
;;;

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

;;;
;;; Formatting Helpers
;;;

(define (format-location location)
  "Format LOCATION as a human-readable string like \"file:line:column\".
Line numbers are converted to 1-indexed.  Returns an empty string if
no location is available."
  (if (and location (list? location))
      (let ((filename (assoc-ref location 'filename))
            (line (assoc-ref location 'line))
            (column (assoc-ref location 'column)))
        (format #f "~a:~a:~a"
                (or filename "<unknown>")
                (if line (1+ line) "?")
                (or column "?")))
      ""))

(define (pretty-string obj)
  (format #f "~y" obj))

(define (format-assertion-failure-detail assertion-run)
  "Format extra detail for a failing ASSERTION-RUN.

For binary predicate assertions, show the evaluated operands and the
predicate name.  Otherwise, fall back to the returned value when one
is available."
  (let* ((assertion (assoc-ref assertion-run 'assertion))
         (assert-body (assoc-ref assertion 'assertion/body))
         (args-thunk (assoc-ref assertion 'assertion/args-thunk))
         (run-result (assoc-ref assertion-run 'assertion-run/result)))
    (if (and (list? assert-body) (= 3 (length assert-body)))
        (let ((args-run-result
               (running:with-exception-continuation args-thunk)))
          (cond
           ((running:returned? args-run-result)
            (match (running:returned-value args-run-result)
              ((first second)
               (format #f "\n~a and\n~a are not ~a"
                       (pretty-string first)
                       (pretty-string second)
                       (car assert-body)))))
           ((running:raised? args-run-result)
            (format #f "Evaluation of arguments thunk failed with:\n~a"
                    (running:raised-exception args-run-result)))
           (else #f)))
        (and (running:returned? run-result)
             (running:returned-value run-result)))))

(define (pre-evaled-expression message)
    (let* ((assert-body (chain-and message
                          (assoc-ref _ 'assertion)
                          (assoc-ref _ 'assertion/body)))
           (args-thunk (chain-and message
                         (assoc-ref _ 'assertion)
                         (assoc-ref _ 'assertion/args-thunk)))
           (run-result (chain-and message
                         (assoc-ref _ 'assertion-run)
                         (assoc-ref _ 'assertion-run/result))))
      (if (list? assert-body)
          (let ((args-run-result
                 (running:with-exception-continuation args-thunk)))
            (cond
             ((running:returned? args-run-result)
              (cons (car assert-body)
                    (running:returned-value args-run-result)))
             ((running:raised? args-run-result)
              (format #f "Evaluation of arguments thunk failed with:\n~a"
                      (running:raised-exception args-run-result)))
             (else #f)))
          (and (running:returned? run-result)
               (running:returned-value run-result)))))

(define (format-assertion message pass-formatter)
  (let ((assertion-run (assoc-ref message 'assertion-run))
        (outcome (chain-and message
                   (assoc-ref _ 'assertion-run)
                   (assoc-ref _ 'assertion-run/outcome)))
        (run-result (chain-and message
                      (assoc-ref _ 'assertion-run)
                      (assoc-ref _ 'assertion-run/result)))
        (assert-body (chain-and message
                       (assoc-ref _ 'assertion)
                       (assoc-ref _ 'assertion/body)))
        (assert-location (chain-and message
                           (assoc-ref _ 'assertion)
                           (assoc-ref _ 'assertion/location))))
    (case outcome
      ((pass)
       (pass-formatter assert-body))
      ((fail)
       (format #f "~a\n~y✗ ~a\n"
               (format-location assert-location)
               assert-body
               (format-assertion-failure-detail assertion-run)))
      ((error)
       (format #f "~a\n~y✗ produced error:\n ~s\n"
               (format-location assert-location)
               assert-body
               (exception->string
                (and (running:raised? run-result)
                     (running:raised-exception run-result)))))
      (else #f))))

(define (format-assertion-minimal message)
  (format-assertion message (lambda (_) "✓")))

(define (format-assertion-verbose message)
  (format-assertion message
                    (lambda (assert-body)
                      (format #f "~y✓\n" assert-body))))

;;;
;;; Test Formatting
;;;

(define (format-test-compact test)
  "Format TEST as a compact single-line string: just the description."
  (or (assoc-ref test 'test/description) "<unnamed test>"))

(define (format-test-twoline test)
  "Format TEST as two lines: description and source location."
  (let ((desc (or (assoc-ref test 'test/description) "<unnamed test>"))
        (loc (format-location (assoc-ref test 'test/location))))
    (if (string-null? loc)
        desc
        (format #f "~a\n  ~a" desc loc))))

(define (format-test-verbose test)
  "Format TEST as a multi-line verbose string with description,
location, and metadata."
  (let* ((desc (format-test-compact test))
         (loc (format-location (assoc-ref test 'test/location)))
         (metadata (or (assoc-ref test 'test/metadata) '())))
    (string-append
     desc "\n"
     (if (string-null? loc) "" (format #f "  location: ~a\n" loc))
     (if (null? metadata) "" (format #f "  metadata: ~y" metadata)))))


;;;
;;; JUnit XML
;;;

(define (suite-run-summary->attributes suite-run-summary)
  "Convert SUITE-RUN-SUMMARY alist to JUnit testsuite attributes"
  `((tests ,(number->string (assoc-ref suite-run-summary 'tests)))
    (failures ,(number->string (assoc-ref suite-run-summary 'failures)))
    (errors ,(number->string (assoc-ref suite-run-summary 'errors)))
    (skipped ,(number->string (assoc-ref suite-run-summary 'skipped)))
    (assertions ,(number->string (assoc-ref suite-run-summary 'assertions)))))

(define (test-run-summary->attributes test-run-summary)
  "Convert TEST-RUN-SUMMARY alist to JUnit testcase attributes"
  `((assertions ,(number->string (assoc-ref test-run-summary 'assertions)))))

(define (node->junit-sxml node classname-path)
  "Convert a single node (suite or test) to JUnit SXML"
  (cond
   ;; Suite node
   ((and (assoc-ref node 'suite)
         (assoc-ref node 'suite-node/children))
    (let* ((suite (assoc-ref node 'suite))
           (suite-name (assoc-ref suite 'suite/description))
           (children (assoc-ref node 'suite-node/children))
           (suite-run-summary (assoc-ref node 'suite-run/summary))
           (new-classname-path
            (if (null? classname-path)
                suite-name
                (string-append classname-path "." suite-name)))
           (attributes (if suite-run-summary
                           (cons `(name ,suite-name)
                                 (suite-run-summary->attributes
                                  suite-run-summary))
                           `((name ,suite-name)))))
      `(testsuite (@ ,@attributes)
                  ,@(map (lambda (child)
                           (node->junit-sxml child new-classname-path))
                         children))))

   ;; Test node
   ((assoc-ref node 'test)
    (let* ((test (assoc-ref node 'test))
           (test-name (assoc-ref test 'test/description))
           (test-run-summary (assoc-ref node 'test-run/summary))
           (test-outcome (assoc-ref node 'test-run/outcome))
           (attributes (append
                        `((name ,test-name)
                          (classname ,classname-path))
                        (if test-run-summary
                            (test-run-summary->attributes test-run-summary)
                            '())))
           (status-element (case test-outcome
                             ((error)
                              '((error (@ (message "Test had errors")
                                          (type "TestError")))))
                             ((fail)
                              '((failure (@ (message "Test failed")
                                            (type "AssertionError")))))
                             (else '()))))
      `(testcase (@ ,@attributes)
                 ,@status-element)))

   (else '())))

(define (calculate-totals forest)
  "Calculate total statistics from all top-level suites"
  (fold (lambda (node acc)
          (let ((suite-run-summary (assoc-ref node 'suite-run/summary)))
            (if suite-run-summary
                (map (lambda (key-val)
                       (match key-val
                         ((key val)
                          (cons key (+ val (assoc-ref suite-run-summary key))))))
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
                                (suite-run-summary->attributes totals))))
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

;;;
;;; Tree-style formatting (like the tree CLI command)
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
