#! /usr/bin/env -S guile -e '(ares scripts ares-suitbl)' -s
!#
;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2026 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (ares scripts ares-suitbl)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (ares version)
  #:export (main))

(define %usage
  (format #f "Usage: ~a [OPTION]... [-- GUILE_OPTION]...

  -r, --reporter=EXPR  use EXPR as test reporter
                       (default: base; or dots, dots-with-hierarchy,
                       minimal, silent, or any Scheme expression)
  -v, --version        display version information and exit
  -h, --help           display this help and exit
"
          (car (command-line))))

(define %builtin-reporters
  '((base . (@ (ares suitbl reporters) test-reporter-base))
    (dots . (@ (ares suitbl reporters) test-reporter-dots))
    (dots-with-hierarchy . (@ (ares suitbl reporters)
                              test-reporter-dots-with-hierarchy))
    (minimal . (@ (ares suitbl reporters) test-reporter-minimal))
    (silent . (@ (ares suitbl reporters) test-reporter-silent))))

(define (get-reporter-expr reporter-name)
  "Return reporter expression for REPORTER-NAME.
If REPORTER-NAME matches a built-in, return the module reference.
Otherwise, read it as an arbitrary Scheme expression."
  (define builtin
    (assoc (string->symbol reporter-name) %builtin-reporters))
  (if builtin
      (cdr builtin)
      (with-input-from-string reporter-name read)))

(define %options
  (list (option '(#\v "version") #f #f
                (lambda _
                  (format #t "~a ~a~%" (car (command-line)) ares-version)
                  (exit 0)))
        (option '(#\h "help") #f #f
                (lambda _
                  (display %usage)
                  (newline)
                  (exit 0)))
        (option '(#\r "reporter") #t #f
                (lambda (opt name arg loads)
                  (acons 'reporter arg loads)))))

(define (main args)
  (define options
    (args-fold (cdr args)
               %options
               (lambda (opt name arg loads)
                 (format (current-error-port)
                         "Unrecognized option '~a'~%~%" name)
                 (display %usage)
                 (newline)
                 (exit 1))
               (lambda (op loads)
                 (acons 'guile-arg op loads))
               '()))

  (define reporter-name (or (assoc-ref options 'reporter) "base"))
  (define reporter-expr (get-reporter-expr reporter-name))

  (define run-code
    `(begin
       (use-modules (ares suitbl core)
                    (ares suitbl runners)
                    (ares suitbl reporters)
                    (ares suitbl ares))
       (define runner
         (make-suitbl-test-runner
          #:config (list (cons 'test-reporter ,reporter-expr))))
       (parameterize ((test-runner* runner))
         (load-project-tests)
         (runner '((type . runner/run-tests))))
       (define summary (runner '((type . runner/get-run-summary))))
       (if (and summary
                (zero? (+ (or (assoc-ref summary 'failures) 0)
                          (or (assoc-ref summary 'errors) 0))))
           (exit 0)
           (exit 1))))

  (define guile-args
    (fold (lambda (option l)
            (if (eq? 'guile-arg (car option))
                (cons (cdr option) l)
                l))
          '()
          options))

  (apply
   execlp
   `("guile"
     ,(car args)
     ,@guile-args
     "-c"
     ,(format #f "~s" run-code))))
