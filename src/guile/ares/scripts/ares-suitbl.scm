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

  -r, --reporter=EXPR  evaluate EXPR in the context of (ares suitbl reporters)
                       e.g. -r base, -r dots,
                            -r '(reporter-every (list dots run-summary))'
  -s, --scheduler=EXPR evaluate EXPR in the context of (ares suitbl schedulers)
                       e.g. -s fast, -s failed-or-all,
                            -s '(make-matching \"macro\")'
  -v, --version        display version information and exit
  -h, --help           display this help and exit
"
          (car (command-line))))

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
                  (acons 'reporter arg loads)))
        (option '(#\s "scheduler") #t #f
                (lambda (opt name arg loads)
                  (acons 'scheduler arg loads)))))

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

  (define reporter-expr (assoc-ref options 'reporter))
  (define scheduler-expr (assoc-ref options 'scheduler))

  (define run-code
    `(begin
       (use-modules (ares suitbl core)
                    (ares suitbl runners)
                    (ares suitbl reporters)
                    (ares suitbl ares))

       (define reporter-config
         ,(if reporter-expr
              `(list (cons 'test-reporter
                          (eval ',(with-input-from-string reporter-expr read)
                                (resolve-module '(ares suitbl reporters)))))
              ''()))

       (define scheduler-config
         ,(if scheduler-expr
              `(list (cons 'schedule-tests
                          (eval ',(with-input-from-string scheduler-expr read)
                                (resolve-module '(ares suitbl schedulers)))))
              ''()))

       (define runner
         (make-suitbl-test-runner
          #:config (append reporter-config scheduler-config)))

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
