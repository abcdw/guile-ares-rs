;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares-extension ares testing)
  #:use-module (ares alist)
  #:use-module (ares guile)
  #:export (ares.testing))

(define (code->eval-request-context context code)
  (let ((nrepl-message (assoc-ref context 'nrepl/message)))
    (cons `(nrepl/message . ,(append
                              `(("op" . "eval")
                                ("code" . ,code)
                                ("ns" .
                                 ,(or (assoc-ref nrepl-message "module")
                                      "(ares suitbl ares)")))
                              (alist-select-keys
                               '("id" "session")
                               nrepl-message)))
          context)))

(define (run context handler)
  "Run tests loaded into test runner."
  ;; TODO: [Andrew Tropin, 2025-08-14] Try to use top-level-handler
  ;; instead of bencodes one.

  ;; (throw 'error-handled-by-some-other-extension)
  (handler (code->eval-request-context
            context "((@ (ares suitbl ares) run-tests))")))

(define (get-test-runner-stats context handler)
  "Return test runner stats."
  (handler (code->eval-request-context
            context
            "((@ (ares suitbl ares) get-current-test-runner-stats))")))

(define (load-module-tests context handler)
  "Load tests corresponding to current module."
  (handler (code->eval-request-context
            context
            "((@ (ares suitbl ares) load-module-tests))")))

(define (load-project-tests context handler)
  "Load all project tests."
  (handler (code->eval-request-context
            context
            "((@ (ares suitbl ares) load-project-tests))")))

(define (get-filtered-tests context handler)
  "Takes a regexp and returns a list of strings representing matched
tests."
  (let* ((message (assoc-ref context 'nrepl/message))
         (pattern (assoc-ref message "ares.testing/filter-pattern"))
         (reply! (assoc-ref context 'reply!)))
    (handler (code->eval-request-context
              context
              (format
               #f
               "((@ (ares suitbl ares) get-filtered-tests) ~s)"
               pattern)))))

(define operations
  `(("ares.testing/run" . ,run)
    ("ares.testing/get-filtered-tests" . ,get-filtered-tests)
    ("ares.testing/get-test-runner-stats" . ,get-test-runner-stats)
    ("ares.testing/load-module-tests" . ,load-module-tests)
    ("ares.testing/load-project-tests" . ,load-project-tests)))

(define-with-meta (ares.testing handler)
  "Add integration with suitbl testing library and corresponding
operations."
  `((provides . (ares.testing))
    (requires . (nrepl.session))
    (wraps . (nrepl.evaluation))
    (handles . ,operations))
  (lambda (context)
    (let* ((message (assoc-ref context 'nrepl/message))
           (op (assoc-ref message "op"))
           (operation-function (assoc-ref operations op)))
      (if operation-function
          (operation-function context handler)
          (handler context)))))
