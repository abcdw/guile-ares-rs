;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2025 Libre en Communs <contact@a-lec.org>
;;; Copyright © 2025 Noé Lopez <noelopez@free.fr>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (2025-07-23-thread-interruption)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-64)
  #:use-module (test-utils))

(test-begin "")

(test-group "thread interruption"
  (define thread
    (call-with-new-thread
     (lambda ()
       (let loop () (loop)))))

  (test-assert (not (thread-exited? thread)))

  (cancel-thread thread 'beep)

  (test-equal 'beep (join-thread thread))
  (test-assert (thread-exited? thread)))

(test-group "thread interruption doesnt trigger prompt"
  (define tag (make-prompt-tag "ares-interrupt-tag"))
  (define thread
    (call-with-new-thread
     (lambda ()
       (call-with-prompt tag
         (lambda ()
           (let loop () (loop)))
         (lambda (kont)
           'aborted)))))

  (test-assert (not (thread-exited? thread)))

  (cancel-thread thread 'beep)

  (test-equal 'beep (join-thread thread))
  (test-assert (thread-exited? thread)))

(test-group "thread interruption to prompt"
  (define tag (make-prompt-tag "ares-interrupt-tag"))
  (define thread
    (call-with-new-thread
     (lambda ()
       (call-with-prompt tag
         (lambda ()
           (let loop () (loop)))
         (lambda (kont)
           'aborted)))))

  (test-assert (not (thread-exited? thread)))

  (system-async-mark
   (lambda ()
     (format #t "inside mark~%")
     (abort-to-prompt tag))
   thread)

  (test-equal 'aborted (join-thread thread))
  (test-assert (thread-exited? thread)))

(test-end "")
