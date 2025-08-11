;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares-extension ares testing)
  #:use-module (ares alist)
  #:use-module (ares guile)
  #:export (ares.testing))

(define (run-tests-eval-request context)
  (append
   `(("op" . "eval")
     ("code" . "((@ (ares suitbl) rn-tests))")
     ("ns" . "(ares suitbl)"))
   (alist-select-keys
    '("id" "session")
    (assoc-ref context 'nrepl/message))))

(define (context->run-test context)
  (cons `(nrepl/message . ,(run-tests-eval-request context))
   context))

(define (run context handler)
  "Run tests loaded into test runner."
  (let ((new-context (context->run-test context)))
    (handler new-context)))

(define operations
  `(("ares.testing/run" . ,run)))

(define-with-meta (ares.testing handler)
  "Add integration with suitbl testing library and corresponding
operations."
  `((provides . (ares.testing))
    (requires . (ares.core ares.transport))
    (handles . ,operations))
  (lambda (context)
    (let* ((message (assoc-ref context 'nrepl/message))
           (op (assoc-ref message "op"))
           (operation-function (assoc-ref operations op)))
      (if operation-function
          (operation-function context handler)
          (handler context)))))
