;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2025 Libre en Communs <contact@a-lec.org>
;;; Copyright © 2025 Noé Lopez <noelopez@free.fr>
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

(define-module (ares evaluation thread-manager-test)
  #:use-module (ares guile prelude)
  #:use-module (ares evaluation thread-manager)
  #:use-module (ares evaluation test-utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (test-utils))

;; Channel use to synchronise evaluation thread with tests. The
;; evaluation simply writes a message to the channel and the test can
;; wait for it before starting.
(define sync-channel (make-channel))

(define-test test-new-evaluation-thread-manager
  (define (send channel message)
    (quickly (put-operation channel message)))
  (define (check-reply name channel expected)
    (test-equal name
                expected
                (quickly (get-operation channel))))
  (define (check-replies name channel expected)
    "Checks multiple replies, ignoring order"
    (let ((replies
           (map
            (lambda _ (quickly (get-operation channel)))
            expected)))
      (test-assert name (lset= equal? replies expected))))

  (test-group
   "Testing new evaluation thread manager"
   (run-fibers
    (lambda ()
      (define channel (make-channel))
      (define reply-channel (make-channel))
      (define reply! (lambda (reply) (put-message reply-channel reply)))
      (define* (send-eval channel code #:key (ns #f))
        (send channel `(("op" . "eval")
                        ("code" . ,code)
                        ,@(if ns `(("ns" . ,(object->string ns))) '())
                        ("reply!" . ,reply!))))
      (spawn-fiber
       (lambda ()
         (evaluation-thread-manager
          channel)))

      (send-eval channel "(+ 3 2)")
      (check-reply "eval addition" reply-channel
                  `(("value" . "5")
                    ("status" . #("done"))))

      (send-eval channel "(read)")
      (check-reply "need input" reply-channel
                  `(("status" . #("need-input"))))
      (send channel `(("op" . "stdin")
                      ("stdin" . "(hello world !)")
                      ("reply!" . ,reply!)))
      (check-replies "received input" reply-channel
                     '((("value" . "(hello world !)")
                        ("status" . #("done")))
                       (("status" . #("done")))))

      (let ((module '(srfi srfi-1)))
        (send-eval channel "(module-name (current-module))" #:ns module)
        (check-reply "current module is set according to ns" reply-channel
                    `(("value" . ,(object->string module))
                      ("status" . #("done")))))

      (send-eval channel "(format #t \"beep beep I'm a sheep!\")")
      (check-reply "received output" reply-channel
                  `(("out" . "beep beep I'm a sheep!")))
      (check-reply "format to stdout" reply-channel
                  `(("value" . "#t")
                    ("status" . #("done"))))

      (send-eval channel "(format (current-error-port) \"beep beep I'm a sheep!\")")
      (check-reply "received error output" reply-channel
                  `(("err" . "beep beep I'm a sheep!")))
      (check-reply "format to error port" reply-channel
                  `(("value" . "#t")
                    ("status" . #("done"))))

      (send-eval channel "(format (current-warning-port) \"beep beep I'm a sheep!\")")
      (check-reply "received warning output" reply-channel
                  `(("err" . "beep beep I'm a sheep!")))
      (check-reply "format to warning output" reply-channel
                  `(("value" . "#t")
                    ("status" . #("done"))))

      (send-eval channel "(define kont #f)(call/cc (lambda (k) (set! kont k) 5))")
      (check-reply "set continuation" reply-channel
                  `(("value" . "5")
                    ("status" . #("done"))))
      (send-eval channel "(kont 42)")
      (check-reply "set continuation" reply-channel
                  `(("value" . "42")
                    ("status" . #("done"))))
      (send-eval channel "(kont 'beep)")
      (check-reply "set continuation" reply-channel
                  `(("value" . "beep")
                    ("status" . #("done"))))

      (send-eval channel "(begin
(display \"hi-err\" (current-error-port))
(force-output (current-error-port))
(display \"hi-out\")
'code-value)")
      (check-reply "received message hi-err" reply-channel
                  `(("err" . "hi-err")))
      (check-reply "received message hi-out" reply-channel
                  `(("out" . "hi-out")))
      (check-reply "received evaluation result" reply-channel
                  `(("value" . "code-value")
                    ("status" . #("done"))))

      (send-eval channel "(begin
(display \"before sleep\")
(force-output (current-output-port))
(sleep 10)
(display \"after sleep\")
'code-value)")
      (check-reply "received message before sleep" reply-channel
                  `(("out" . "before sleep")))
      (send channel `(("op" . "interrupt")
                      ("reply!" . ,reply!)))
      (check-reply "received interrupt done" reply-channel
                  `(("status" . #("done" "interrupted"))))
      (check-reply "received evaluation interrupt" reply-channel
                  `(("status" . #("done" "interrupted"))))

      (send-eval channel "(put-message sync-channel 'ready)(let loop ((a 0)) (loop (1+ a)))"
                 #:ns '(ares evaluation thread-manager-test))
      (test-equal 'ready (quickly (get-operation sync-channel)))
      (send channel `(("op" . "interrupt")
                      ("reply!" . ,reply!)))
      (check-reply "interruption done" reply-channel
                  `(("status" . #("done" "interrupted"))))
      (check-reply "interrupted evaluation" reply-channel
                  `(("status" . #("done" "interrupted"))))
      (send-eval channel "(+ 3 4)")
      (check-reply "evaluation after interrupt" reply-channel
                  `(("value" . "7")
                    ("status" . #("done"))))

      (send channel `(("op" . "interrupt")
                      ("reply!" . ,reply!)))
      (check-reply "idle interruption" reply-channel
                  `(("status" . #("done" "session-idle"))))

      (send-eval channel "(put-message sync-channel 'ready)(let loop ((a 0)) (loop (1+ a)))"
                 #:ns '(ares evaluation thread-manager-test))
      (test-equal 'ready (quickly (get-operation sync-channel)))
      ((@ (ice-9 threads) call-with-new-thread)
       (lambda ()
         (check-reply "interruption waiting for async mark" reply-channel
                     `(("status" . #("done" "interrupting"))))
         (check-reply "interruption done" reply-channel
                     `(("status" . #("done" "interrupted"))))
         (check-reply "interrupted evaluation" reply-channel
                     `(("status" . #("done" "interrupted"))))))
      ;; The first one interrupts, goes to code of after
      ;; evaluation. Second one fires and sends session-idle
      ;; immediately. After evaluation finishes and sends interrupted.
      (send channel `(("op" . "interrupt")
                      ("reply!" . ,reply!)))
      (send channel `(("op" . "interrupt")
                      ("reply!" . ,reply!))))
    #:drain? #t)))
