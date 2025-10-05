;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl discovery)
  #:use-module (ares suitbl core)
  #:use-module ((ice-9 ftw) #:select (nftw))
  #:use-module ((ice-9 regex) #:select (string-match))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-197)
  #:export (get-test-module
            get-module-suites
            get-module-public-suites
            get-all-test-modules))


;;;
;;; Test Discovery API
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

;; TODO: [Andrew Tropin, 2025-08-15] Ensure the module is reloaded and
;; stale tests are cleaned up
(define (get-module-suites module)
  (filter
   identity
   (module-map (lambda (k v)
                 (and (variable-bound? v)
                      (suite-thunk? (variable-ref v))
                      (variable-ref v)))
               module)))

(define (get-module-public-suites module)
  (or
   (chain-and module
     (module-public-interface _)
     (get-module-suites _))
   '()))

;; TODO: [Andrew Tropin, 2025-10-06] Integrate with test runner and
;; test reporter, so we can control the debug output using test
;; reporters.
(define* (load-test-modules-thunk
          #:key
          (test-file-pattern ".*-test(\\.scm|\\.ss)")
          (load-file (lambda (p rp)
                       (format (current-error-port)
                               "loading test module: ~a\n" p)
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
            (let ((relative-path (string-drop file-path (1+ (string-length path)))))
              (when (string-match test-file-pattern file-path)
                (save-module-excursion
                 (lambda ()
                   (load-file file-path relative-path))))))
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
