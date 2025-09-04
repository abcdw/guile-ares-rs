;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl ares)
  #:use-module ((ares guile prelude) #:select (comment))
  #:use-module ((ares suitbl core) #:select (suite test-runner*))
  #:use-module ((ares suitbl runners)
                #:select (get-loaded-tests
                          get-stats
                          set-run-config-value!))
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
  ((test-runner*) `((type . run-tests))))

(define (get-current-test-runner-stats)
  (chain ((test-runner*) `((type . get-state)))
    (get-stats _)))

(define (load-module-tests)
  (set-run-config-value! ((test-runner*) `((type . get-state))) 'auto-run? #f)
  (let ((m (current-module)))
    (suite (format #f "~a module tests" (module-name m))
      (for-each
       (lambda (ts) (ts))
       (get-module-public-suites (get-test-module (module-name m))))))
  (set-run-config-value! ((test-runner*) `((type . get-state))) 'auto-run? #t)
  *unspecified*)

(define (load-project-tests)
  (set-run-config-value! ((test-runner*) `((type . get-state))) 'auto-run? #f)
  (let ((test-modules (get-all-test-modules)))
    (suite "project tests"
      (for-each
       (lambda (ts) (ts))
       (append-map get-module-public-suites test-modules))))
  (set-run-config-value! ((test-runner*) `((type . get-state))) 'auto-run? #t)
  *unspecified*)

(define (add-indicies tests)
  (map (lambda (t i) (cons `(test/index . ,i) t))
       tests (iota (length tests))))

(define (get-current-test-runner-loaded-test)
  (chain ((test-runner*) `((type . get-state)))
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
