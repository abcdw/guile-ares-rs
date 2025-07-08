#! /usr/bin/env -S guile -e '(ares scripts ares)' -s
!#
;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2025 Libre en Communs <contact@a-lec.org>
;;; Copyright © 2025 Noé Lopez <noelopez@free.fr>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (ares scripts ares)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (ares version)
  #:use-module (ares server)
  #:export (main))

(define %usage
  (format #f "Usage: ~a [OPTION]... [-- GUILE_OPTION]...

  --host=HOST     listen on HOST
  -p, --port=PORT listen on PORT
  --no-port-file  do not create port file
  --port-file-name=NAME write port number to a file at NAME

  -h, --help      display this help and exit
  -v, --version   display version information and exit
"
          (car (command-line))))

(define %options
  (list (option '(#\v "version") #f #f
                (lambda _
                  (format #t "~a ~a~%" (car (command-line)) %ares-version)
                  (exit 0)))
        (option '(#\h "help") #f #f
                (lambda _
                  (display %usage)
                  (exit 0)))
        (option '("host") #t #f
                (lambda (opt name arg loads)
                  (acons 'host arg loads)))
        (option '(#\p "port") #t #f
                (lambda (opt name arg loads)
                  (acons 'port (string->number arg) loads)))
        (option '("no-port-file") #f #f
                (lambda (opt name arg loads)
                  (acons 'no-port-file? #t loads)))
        (option '("port-file-name") #t #f
                (lambda (opt name arg loads)
                  (acons 'port-file-name arg loads)))))

(define (main args)
  (define options
    (args-fold (cdr args)
               %options
               (lambda (opt name arg loads)
                 (format (current-error-port)
                         "Unrecognized option '~a'~%~%" name)
                 (display %usage)
                 (exit 1))
               (lambda (op loads)
                 (acons 'guile-arg op loads))
               '()))

  (define run-server-code
    `((@ (ares server) run-nrepl-server)
      #:nrepl-port-file? ,(not (assoc-ref options 'no-port-file?))
      ,@(if (assoc-ref options 'host) (list #:host (assoc-ref options 'host)) '())
      ,@(if (assoc-ref options 'port) (list #:port (assoc-ref options 'port)) '())
      ,@(if (assoc-ref options 'port-file-name)
            (list #:nrepl-port-path (assoc-ref options 'port-file-name))
            '())))

  (define guile-args
    (fold
     (lambda (option l)
       (if (equal? 'guile-arg (car option))
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
     ,(format #f "~s" run-server-code))))
