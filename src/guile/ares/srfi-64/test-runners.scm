;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023, 2024 Andrew Tropin <andrew@trop.in>
;;;
;;; This file is part of guile-ares-rs.
;;;
;;; guile-ares-rs is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; guile-ares-rs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guile-ares-rs.  If not, see <http://www.gnu.org/licenses/>.

(define-module (ares srfi-64 test-runners)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)

  #:export (%previous-runner
            get-test-module
            get-module-tests
            load-project-tests-thunk
            all-test-modules

            rerun-tests
            run-test
            run-module-tests
            run-project-tests
            run-project-tests-cli

            test-runner-summary))

(define (test? proc)
  "Checks if PROC is a test."
  (and (procedure? proc) (procedure-property proc 'srfi-64-test?)))

(define (string-repeat s n)
  "Returns string S repeated N times."
  (fold
   (lambda (_ str)
     (string-append str s))
   ""
   (iota n)))

(define (test-runner-default)
  (let ((runner (test-runner-null)))
    (test-runner-on-group-begin! runner
      (lambda (runner name count)
        (format #t "~a> ~a\n"
                (string-repeat "-" (length (test-runner-group-stack runner)))
                name)))
    (test-runner-on-group-end! runner
      (lambda (runner)
        (format #t "<~a ~a\n"
                (string-repeat "-"
                               (1- (length (test-runner-group-stack runner))))
                (car (test-runner-group-stack runner)))))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (format #t "[~a] ~a\n"
                (test-result-ref runner 'result-kind)
                (if (test-result-ref runner 'test-name)
                    (test-runner-test-name runner)
                    "<>"))
        (case (test-result-kind runner)
          ((fail)
           (if (test-result-ref runner 'expected-value)
               (format #t "~a:~a\n -> expected: ~s\n -> obtained: ~s\n\n"
                       (test-result-ref runner 'source-file)
                       (test-result-ref runner 'source-line)
                       (test-result-ref runner 'expected-value)
                       (test-result-ref runner 'actual-value))))
          (else #t))))
    (test-runner-on-final! runner
      (lambda (runner)
        (format #t "Source: ~a\nAsserts: pass = ~a, xfail = ~a,\
 xpass = ~a, fail = ~a\n\n"
                (test-result-ref runner 'source-file)
                (test-runner-pass-count runner)
                (test-runner-xfail-count runner)
                (test-runner-xpass-count runner)
                (test-runner-fail-count runner))))
    runner))


;;; Run project tests

(define (get-test-module)
  ;; TODO: Handle the case, when test module doesn't exist.
  "Return a test module related to the current one.  Usually it's a module with
-test prefix.  Return current module if it already contains -test prefix."
  (let* ((m-name (module-name (current-module)))
         (m-tail (last m-name))
         (test-m-tail
          (if (string-suffix? "-test" (symbol->string m-tail))
              m-tail
              (symbol-append m-tail '-test))))
    (resolve-module
     (append
      (drop-right m-name 1)
      (list test-m-tail)))))

(test-runner-factory test-runner-default)

(define (test-runner-summary runner)
  "Return alist of helpful statistics for the test-runner RUNNER."
  `((pass . ,(test-runner-pass-count runner))
    (xfail . ,(test-runner-xfail-count runner))
    (xpass . ,(test-runner-xpass-count runner))
    (fail . ,(test-runner-fail-count runner))))

(define (test-runner-test-results-stack runner)
  (or (assoc-ref (test-runner-aux-value runner) 'test-results-stack)
      '()))

(define (test-runner-test-results runner)
  (reverse (test-runner-test-results-stack runner)))

(define (record-test-run-result runner old-fail-count t)
  (let* ((aux-value (test-runner-aux-value runner))
         (test-result (if (< old-fail-count (test-runner-fail-count runner))
                          `((test . ,t) (status . fail))
                          `((test . ,t) (status . pass))))
         (test-results-stack (test-runner-test-results-stack runner))
         (new-test-results (cons test-result test-results-stack)))
    (test-runner-aux-value! runner
     (assoc-set! aux-value 'test-results-stack new-test-results))))

(define* (run-test
          t
          #:key (runner (test-runner-create)))
  (let ((old-fail-count (test-runner-fail-count runner)))
    (test-with-runner runner
      (t)
      (record-test-run-result runner old-fail-count t)))
  runner)

(define (get-module-tests module)
  (filter
   identity
   (module-map (lambda (k v)
                 (and (variable-bound? v)
                      (test? (variable-ref v))
                      (variable-ref v)))
               module)))

(define* (run-module-tests
          module
          #:key (runner (test-runner-create)))
  (define module-tests (get-module-tests module))
  ;; TODO: Load test module if it's not loaded yet.
  ;; (reload-module module)
  (test-with-runner runner
    (let ((test-name (format #f "module ~a" (module-name module))))
      (test-group test-name
        (map (lambda (t) (run-test t #:runner runner)) module-tests))))
  runner)

(define* (load-project-tests-thunk
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

(define* (all-test-modules
          #:key
          (load-project-test-modules? #f)
          (load-project-tests (load-project-tests-thunk)))
  (when load-project-test-modules?
    (load-project-tests))
  (filter (lambda (m)
            (string-suffix? "-test" (symbol->string (last (module-name m)))))
          ((@ (ares reflection modules) all-modules))))

(define* (run-project-tests
          #:key
          (runner (test-runner-create))
          (test-modules (all-test-modules #:load-project-test-modules? #t)))
  (test-with-runner runner
    (test-group "PROJECT TEST"
      (map (lambda (m)
             (let ((module-tests (get-module-tests m)))
               (when (not (null? module-tests))
                 (run-module-tests m #:runner runner))))
           test-modules)))
  runner)

;; (test-runner-summary (run-project-tests))
(define (run-project-tests-cli)
  (let* ((summary (test-runner-summary (run-project-tests)))
         (fail-count (assoc-ref summary 'fail)))
    (exit (zero? fail-count))))

(define* (rerun-tests
          previous-runner
          #:key
          (runner (test-runner-create))
          (filter-fn (const #t)))
  (when previous-runner
    (let* ((test-results (test-runner-test-results previous-runner))
           (get-test (lambda (x) (assoc-ref x 'test)))
           (filtered-tests (map get-test (filter filter-fn test-results))))
      (test-with-runner runner
        (test-group "RERUN TESTS"
          (map (lambda (t) (run-test t #:runner runner)) filtered-tests)))))
  runner)


;; (format #t "~y" (all-test-modules))

;; (load-project-tests)

;; (set! %previous-runner (run-project-tests))
;; (rerun-tests %previous-runner
;;              #:filter-fn (lambda (x) (equal? 'fail (assoc-ref x 'status))))

;; (module-clear! (resolve-module '(rde serializers nginx-test)))
;; (get-module-tests (resolve-module '(rde serializers nginx-test)))

;; (run-module-tests (resolve-module '(rde serializers nginx-test)))

;; (use-modules (ice-9 pretty-print))

;; (rerun-tests %previous-runner)
;; (define prev-runner (test-runner-default))

;; (let ((runner %previous-runner))
;;   (run-module-tests
;;    (resolve-module '(rde serializers nginx-test))
;;    #:runner runner)
;;   (pretty-print (test-runner-test-results runner)))

;; (re-run-failed-tests prev-runner)

;; https://www.mail-archive.com/geiser-users%40nongnu.org/msg00323.html
;; https://rednosehacker.com/revisiting-guile-xunit

;; Test runners:
;; https://github.com/aconchillo/guile-json/blob/master/tests/runner.scm
;; https://luis-felipe.gitlab.io/guile-proba/
;; https://git.systemreboot.net/run64/tree/bin/run64
;; https://framagit.org/Jeko/guile-spec

;; Common lisp testing frameworks:
;; https://sabracrolleton.github.io/testing-framework

;; Clojure testing libraries:
;; https://jakemccrary.com/blog/2014/06/22/comparing-clojure-testing-libraries-output/

;; Scheme testing libraries:
;; https://github.com/tali713/mit-scheme/blob/master/tests/unit-testing.scm
;; https://code.call-cc.org/svn/chicken-eggs/release/5/test/trunk/test.scm

;; TODO:
;; - Make test-assert to show line, where it fails.
;; - Implement test-match, which uses ice-9 match like patterns and provides
;;   meaningful report.


;; https://docs.racket-lang.org/rackunit/api.html
