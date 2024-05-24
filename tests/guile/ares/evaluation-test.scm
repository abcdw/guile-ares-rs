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

(define-module (ares evaluation-test)
  #:use-module (ares evaluation)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers timers)
  #:use-module (fibers conditions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (test-utils))

(define (alist-include? template data)
  "Check if all of the TEMPLATE's key-values pairs are present in DATA."
  (lset<= equal? template data))

(define* (lset-contains?
          obj list
          #:key (predicate equal?))
  (lset<= predicate `(,obj) list))

(define* (quickly operation
                  #:key
                  (timeout 1)
                  (default-value #f))
  (perform-operation
   (choice-operation
    (wrap-operation
     (sleep-operation timeout)
     (const default-value))
    operation)))

(define-test test-output-stream-manager
  (test-group "Testing Output Stream Manager"
    (run-fibers
     (lambda ()
       (let* ((message-channel (make-channel))
              (finished-condition (make-condition))
              (process-finished-condition (make-condition))
              (stdout-pipe (pipe))
              (stdout-input-port (car stdout-pipe))
              (stdout-output-port (cdr stdout-pipe))
              (_ (setvbuf stdout-output-port 'none)))

         (spawn-fiber
          (output-stream-manager-thunk stdout-input-port
                                       identity
                                       message-channel
                                       process-finished-condition
                                       #:finished-condition finished-condition))

         (spawn-fiber
          (lambda ()
            (parameterize ((current-output-port stdout-output-port))
              (display "hi"))
            (signal-condition! process-finished-condition)))

         (test-equal "Received message hi"
           "hi" (get-message message-channel))

         (close-port stdout-input-port)
         (close-port stdout-output-port)

         (test-assert "Pipe Closed (input)"
           (port-closed? stdout-input-port))
         (test-assert "Pipe Closed (output)"
           (port-closed? stdout-output-port))

         (test-assert "Output Stream Manager Finished"
           (quickly
            (wrap-operation
             (wait-operation finished-condition)
             (const #t)))))))
    #:drain? #t))

(define-test test-evaluation-thread-manager
  (test-group "Testing Evaluation Thread Manager"
    (run-fibers
     (lambda ()
       (let* ((replies-channel (make-channel))
              (command-channel (make-channel)))
         (spawn-fiber
          (evaluation-thread-manager-thunk command-channel))

         (define* (run-eval code
                            #:optional
                            (finished-condition (make-condition))
                            #:key (ns #f))
           (put-message
            command-channel
            `((action . evaluate)
              (message . (,@(if ns `(("ns" . ,(object->string ns))) '())
                          ("op" . "eval")
                          ("code" . ,(format #f "~s" code))))
              (replies-channel . ,replies-channel)
              (evaluation-finished . ,finished-condition))))

         (let ((finished (make-condition))
               (module '(srfi srfi-1))
               (test-name "Check current-module is set according to ns"))
           (test-begin test-name)
           (run-eval
            `(module-name (current-module))
            finished
            #:ns module)

           (test-equal "Received message hi-out"
             `(("value" . ,(object->string module))
               ("status" . #("done")))
             (quickly (get-operation replies-channel)))

           (test-assert "Finished condition signalled"
             (quickly (wrap-operation
                       (wait-operation finished)
                       (const #t))))

           (test-end test-name))

         (let ((finished (make-condition)))
           (test-begin "Simple Evaluation with output streams capture")
           (run-eval
            `(begin
               (display "hi-err" (current-error-port))
               (display "hi-out")
               'code-value)
            finished)

           (define replies
             (map
              (lambda (_)
                (quickly (get-operation replies-channel)))
              (iota 3)))

           (test-assert "Received message hi-err"
             (lset-contains?
              `(("err" . "hi-err"))
              replies))
           (test-assert "Received message hi-out"
             (lset-contains?
              `(("out" . "hi-out"))
              replies))
           (test-assert "Received evaluation result"
             (lset-contains?
              `(("value" . "code-value")
                ("status" . #("done")))
              replies))

           (test-assert "Finished condition signalled"
             (quickly (wrap-operation
                       (wait-operation finished)
                       (const #t))))

           (test-end "Simple Evaluation with output streams capture"))

         (let ((finished (make-condition)))
           (test-begin "Evaluation Interruption")

           (run-eval
            `(begin
               (display "before sleep")
               (sleep 10)
               (display "after sleep")
               'code-value)
            finished)
           (test-equal "Received message hi-out"
             `(("out" . "before sleep"))
             (quickly (get-operation replies-channel)))
           (put-message
            command-channel
            `((action . interrupt)))
           (test-equal "Received evaluation interrupt"
             `(("status" . #("done" "interrupted")))
             (quickly (get-operation replies-channel)))
           (test-assert "Finished condition signalled"
             (quickly (wrap-operation
                       (wait-operation finished)
                       (const #t))))
           (test-end "Evaluation Interruption"))

         (let ((finished (make-condition)))
           (test-begin "Saved continuation evaluation")

           (run-eval `(define kont #f))
           (test-equal "Variable declared"
             `(("value" . "#<unspecified>")
               ("status" . #("done")))
             (quickly (get-operation replies-channel)))

           (run-eval `(+ 1 (call/cc (lambda (k) (set! kont k) 5))))
           (test-equal "Continuation saved and result returned"
             `(("value" . "6")
               ("status" . #("done")))
             (quickly (get-operation replies-channel)))

           (run-eval `(kont 41) finished)
           (test-equal "Continuation invoked"
             `(("value" . "42")
               ("status" . #("done")))
             (quickly (get-operation replies-channel)))

           (test-assert "Finished condition signalled"
             (quickly (wrap-operation
                       (wait-operation finished)
                       (const #t))))

           (test-end "Saved continuation evaluation"))

         (let ((finished (make-condition)))
           (test-begin "Saved continuation with stdout")

           (run-eval `(define kont #f))
           (test-equal "Variable declared"
             `(("value" . "#<unspecified>")
               ("status" . #("done")))
             (quickly (get-operation replies-channel)))

           (run-eval `(display (call/cc (lambda (k) (set! kont k) 5))))
           (test-equal "Result printed"
             `(("out" . "5"))
             (quickly (get-operation replies-channel)))
           (test-equal "Continuation saved"
             `(("value" . "#<unspecified>")
               ("status" . #("done")))
             (quickly (get-operation replies-channel)))

           (run-eval `(kont 42) finished)
           (test-equal "Value displayed"
             `(("out" . "42"))
             (quickly (get-operation replies-channel)))
           (test-equal "Continuation invoked"
             `(("value" . "#<unspecified>")
               ("status" . #("done")))
             (quickly (get-operation replies-channel)))

           (test-assert "Finished condition signalled"
             (quickly (wrap-operation
                       (wait-operation finished)
                       (const #t))))

           (test-end "Saved continuation evaluation"))

         (let ((finished (make-condition)))
           (test-begin "Get Input")

           (run-eval `((@ (ice-9 rdelim) read-line)) finished)
           (test-equal "Input requested"
             `(("status" . #("need-input")))
             (quickly (get-operation replies-channel)))
           (put-message
            command-channel
            `((action . provide-input)
              (stdin . "hello\n")))
           (test-equal "Input read"
             `(("value" . "\"hello\"")
               ("status" . #("done")))
             (quickly (get-operation replies-channel)))

           (test-assert "Finished condition signalled"
             (quickly (wrap-operation
                       (wait-operation finished)
                       (const #t))))

           (test-end "Get input")))))))

(define-test test-evaluation-supervisor
  (test-group "Testing Evaluation Supervisor"
    (test-group "Shutdown with command"
      (run-fibers
       (lambda ()
         (let* ((finished-condition (make-condition))
                (command-channel (make-channel)))

           (spawn-fiber
            (evaluation-supervisor-thunk
             command-channel
             #:finished-condition finished-condition))

           (evaluation-supervisor-shutdown command-channel)

           (test-assert "Finish condition signalled"
             (quickly
              (wrap-operation
               (wait-operation finished-condition)
               (const #t))))))))

    (test-group "Shutdown with shutdown-condition"
      (run-fibers
       (lambda ()
         (let* ((finished-condition (make-condition))
                (shutdown-condition (make-condition))
                (command-channel (make-channel)))

           (spawn-fiber
            (evaluation-supervisor-thunk
             command-channel
             #:finished-condition finished-condition
             #:shutdown-condition shutdown-condition))

           (signal-condition! shutdown-condition)

           (test-assert "Finish condition signalled"
             (quickly
              (wrap-operation
               (wait-operation finished-condition)
               (const #t))))))))

    (test-group "Simple evaluation and interruption"
      (run-fibers
       (lambda ()
         (let* ((finished-condition (make-condition))
                (shutdown-condition (make-condition))
                (command-channel (make-channel))
                (replies-channel (make-channel))
                (reply-function (lambda (reply)
                                  (put-message replies-channel reply))))

           (spawn-fiber
            (evaluation-supervisor-thunk
             command-channel
             #:finished-condition finished-condition
             #:shutdown-condition shutdown-condition))

           (evaluation-supervisor-process-nrepl-message
            command-channel
            `(("code" . "#f")
              ("op" . "eval"))
            reply-function)

           (test-equal "Returned #f evaluation value"
             `(("value" . "#f")
               ("status" . #("done")))
             (quickly (get-operation replies-channel)))

           (evaluation-supervisor-process-nrepl-message
            command-channel
            `(("code" . "(])")
              ("op" . "eval"))
            reply-function)

           (quickly (get-operation replies-channel))
           (test-equal "Returned read-error exception"
             "read-error"
             (assoc-ref (quickly (get-operation replies-channel)) "ex"))

           (test-equal "Returned evaluation value"
             `(("status" . #("done")))
             (quickly (get-operation replies-channel)))

           (evaluation-supervisor-process-nrepl-message
            command-channel
            `(("op" . "interrupt"))
            reply-function)

           (test-equal "Interrupt idle session"
             `(("status" . #("session-idle" "done")))
             (quickly (get-operation replies-channel)))

           (evaluation-supervisor-process-nrepl-message
            command-channel
            `(("code" . "(begin (sleep 10) 'after-sleep)")
              ("op" . "eval"))
            reply-function)

           (evaluation-supervisor-process-nrepl-message
            command-channel
            `(("op" . "interrupt"))
            reply-function)

           (test-equal "Interruption op done"
             `(("status" . #("done")))
             (quickly (get-operation replies-channel)))

           (test-equal "Evaluation interrupted"
             `(("status" . #("done" "interrupted")))
             (quickly (get-operation replies-channel)))

           (evaluation-supervisor-process-nrepl-message
            command-channel
            `(("code" . "(+ 1 2)")
              ("op" . "eval"))
            reply-function)

           (evaluation-supervisor-shutdown command-channel)

           (test-assert "Finish condition signalled"
             (quickly
              (wrap-operation
               (wait-operation finished-condition)
               (const #t))))

           (test-equal "Returned evaluation value after shutdown"
             `(("value" . "3")
               ("status" . #("done")))
             (quickly (get-operation replies-channel)))))))

    (test-group "Multiple evaluation"
      (run-fibers
       (lambda ()
         (let* ((finished-condition (make-condition))
                (shutdown-condition (make-condition))
                (command-channel (make-channel))
                (replies-channel (make-channel))
                (reply-function (lambda (reply)
                                  (put-message replies-channel reply))))

           (spawn-fiber
            (evaluation-supervisor-thunk
             command-channel
             #:finished-condition finished-condition
             #:shutdown-condition shutdown-condition))

           (evaluation-supervisor-process-nrepl-message
            command-channel
            `(("code" . "(begin (display 'hi) (+ 1))")
              ("op" . "eval"))
            reply-function)

           (evaluation-supervisor-process-nrepl-message
            command-channel
            `(("code" . "(begin (display 'hi2) (+ 2))")
              ("op" . "eval"))
            reply-function)

           (evaluation-supervisor-shutdown command-channel)

           (test-assert "Finish condition signalled"
             (quickly
              (wrap-operation
               (wait-operation finished-condition)
               (const #t))))

           (quickly (get-operation replies-channel))
           (test-equal "Returned evaluation value after shutdown"
             `(("value" . "1")
               ("status" . #("done")))
             (quickly (get-operation replies-channel)))

           (test-assert "3rd message is empty"
             (quickly (get-operation replies-channel)
                      #:timeout 0
                      #:default-value #t))))))))
