;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl ares)
  #:use-module ((ares guile prelude) #:select (comment))
  #:use-module ((ares suitbl core) #:select (suite test-runner*))
  #:use-module  ((ares suitbl reporters)
                 #:select (test-reporter-minimal))
  #:use-module ((ares suitbl runner-state)
                #:select (get-loaded-tests
                          get-runner-config
                          get-stats
                          set-runner-config-value!))
  #:use-module ((ares suitbl discovery)
                #:select (get-all-test-modules
                          get-module-public-suites
                          get-test-module))
  #:use-module ((srfi srfi-1) #:select (append-map))
  #:use-module ((srfi srfi-197) #:select (chain))
  #:export (run-tests
            get-current-test-runner-stats
            get-filtered-tests
            load-module-tests
            load-project-tests))

(define (run-tests)
  ((test-runner*) `((type . runner/run-tests)
                    (runner/config
                     .
                     ((test-reporter . ,test-reporter-minimal)))))
  ((test-runner*) `((type . runner/get-run-summary))))

(define (get-current-test-runner-stats)
  (let ((state ((test-runner*) `((type . runner/get-state)))))
    (get-stats state (get-runner-config state))))

(define (load-module-suite m)
  (suite (format #f "~a module tests" (module-name m))
    'metadata
    `((module . ,m))
    (for-each (lambda (ts) (ts))
              (get-module-public-suites m))))

(define (with-auto-run-disabled thunk)
  (set-runner-config-value!
   ((test-runner*) `((type . runner/get-state)))
   'auto-run? #f)
  (thunk)
  (set-runner-config-value!
   ((test-runner*) `((type . runner/get-state)))
   'auto-run? #t)
  *unspecified*)

(define (load-module-tests)
  (with-auto-run-disabled
   (lambda ()
     (load-module-suite (get-test-module (module-name (current-module)))))))

(define (load-project-tests)
  (with-auto-run-disabled
   (lambda ()
     (let ((test-modules (get-all-test-modules)))
       (suite "project tests"
         (for-each load-module-suite test-modules))))))

(define (add-indicies tests)
  (map (lambda (t i) (cons `(test/index . ,i) t))
       tests (iota (length tests))))

(define (get-current-test-runner-loaded-test)
  (chain ((test-runner*) `((type . runner/get-state)))
    (get-loaded-tests _)
    (add-indicies _)))

(define (test->string t)
  (string-append
   (number->string
    (assoc-ref t 'test/index))
   ": "
   (assoc-ref t 'test/description)))

(define (filter-transformation pattern)
  "Accept a string pattern, and returns a filtering tranformation."
  (define filter-regexp
    (make-regexp pattern))

  (lambda (l)
    (filter
     (lambda (t)
       (regexp-exec filter-regexp (test->string t)))
     l)))

(define (get-filtered-tests pattern)
  (pk (get-current-test-runner-loaded-test))
  (chain (get-current-test-runner-loaded-test)
    ((filter-transformation pattern) _)
    (map test->string _)))

(comment
 (define selector ".*import.*")
 (define rg (make-regexp selector))
 (use-modules (system vm program))
 (chain (get-current-test-runner-loaded-test)
   ;; ((filter-transformation selector) _)
   (map (lambda (t) (assoc-ref t 'test/body-thunk)) _)
   (car _)
   (program-sources _))
 (use-modules (ares suitbl core))

 (define (our-function number)
   (+ 5 number))

 (suite "just suite"
   (test "some notimportant test"
     (is #t))
   (test "some important test"
     (sleep 1)
     (is (= 10 (our-function 5))))
   (test "some minor test"
     (is #t))))
