;;; guile-nrepl --- Asynchronous Reliable Extensible Scheme RPC Server
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
;;;
;;; This file is part of guile-nrepl.
;;;
;;; guile-nrepl is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; guile-nrepl is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guile-nrepl.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nrepl server evaluation-test)
  #:use-module (nrepl server evaluation)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers timers)
  #:use-module (fibers conditions)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (test-utils))

(define (alist-include? template data)
  "Check if all of the TEMPLATE's key-values pairs are presesnt in DATA."
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
              (stdout-pipe (pipe))
              (stdout-input-port (car stdout-pipe))
              (stdout-output-port (cdr stdout-pipe))
              (_ (setvbuf stdout-output-port 'none)))

         (spawn-fiber
          (output-stream-manager-thunk stdout-input-port
                                       identity
                                       message-channel finished-condition))

         (spawn-fiber
          (lambda ()
            (parameterize ((current-output-port stdout-output-port))
              (display "hi"))
            (signal-condition! finished-condition)))

         (test-equal "Received message hi"
           "hi" (get-message message-channel))

         (close-port stdout-input-port)
         (close-port stdout-output-port)

         (test-assert "Pipe Closed (input)"
           (port-closed? stdout-input-port))
         (test-assert "Pipe Closed (output)"
           (port-closed? stdout-output-port)))))
    #:drain? #t))

(define-test test-reusable-threads
  (define make-reusable-thread
    (@@ (nrepl server evaluation) make-reusable-thread))
  (define reuse-thread
    (@@ (nrepl server evaluation) reuse-thread))

  (test-group "Testing reusable thread"
    (test-group "Basic use case: start, reuse, get channel value, shutdown"
      (run-fibers
       (lambda ()
         (let* ((shutdown-condition (make-condition))
                (th (make-reusable-thread shutdown-condition))
                (ch (make-channel)))
           (reuse-thread th (lambda () (put-message ch 'hello)))
           (signal-condition! shutdown-condition)
           (test-equal "Obtained value from channel" 'hello (get-message ch))
           (join-thread th)
           ;; Otherwise thread is not yet exited.
           (usleep 1)
           (test-assert "Thread exited" (thread-exited? th))))))
    #;
    (test-group "Blocking on signal-condition!"
      (run-fibers
       (lambda ()
         (let* ((shutdown-condition (make-condition))
                (th (make-reusable-thread shutdown-condition))
                (ch (make-channel))
                (obtained-value (make-condition)))
           (sleep 1)
           (reuse-thread th (lambda () (put-message ch 'hello)))
           (test-equal "Obtained value from channel" 'hello (get-message ch))
           (signal-condition! shutdown-condition)
           (pk 'hello) ; not printed
           (test-assert "Thread exited" (thread-exited? th))))))))

(define-test test-evaluation-manager
  (test-group "Testing Evaluation Manager"
    (test-group "Simple Evaluation with output streams capture"
      (define sample-code
        `(begin
           (display "hi-err" (current-error-port))
           (display "hi-out")
           'code-value))

      (run-fibers
       (lambda ()
         (let* ((replies-channel (make-channel)))

           (spawn-fiber
            (evaluation-manager-thunk sample-code replies-channel))

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
              replies))))))

    (test-group "Evaluation Interruption"
      (define sample-code-2
        `(begin
           (display "before sleep")
           (sleep 10)
           (display "after sleep")
           'code-value))
      (run-fibers
       (lambda ()
         (let* ((replies-channel (make-channel))
                (interrupt-condition (make-condition)))

           (spawn-fiber
            (evaluation-manager-thunk
             sample-code-2 replies-channel
             #:interrupt-condition interrupt-condition))

           (test-equal "Received message hi-out"
             `(("out" . "before sleep"))
             (quickly (get-operation replies-channel)))

           (signal-condition! interrupt-condition)

           (test-equal "Received evaluation interrupt"
             `(("status" . #("done" "interrupted")))
             (quickly (get-operation replies-channel)))))))))


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

    (test-group "Simple evaluation and idle interruption"
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
            `(("code" . "(begin (+ 1 2) (display 'hi))")
              ("op" . "eval"))
            reply-function)

           (evaluation-supervisor-process-nrepl-message
            command-channel
            `(("code" . "(begin (+ 1 2) (display 'hi))")
              ("op" . "eval"))
            reply-function)

           (evaluation-supervisor-shutdown command-channel)

           (test-assert "Finish condition signalled"
             (quickly
              (wrap-operation
               (wait-operation finished-condition)
               (const #t))))

           (list
            (quickly (get-operation replies-channel))
            (quickly (get-operation replies-channel))
            (quickly (get-operation replies-channel)))
           ;; 2 eval results, 2 out messages
           ;; (test-expect-fail 1)
           (test-assert "4th message returned"
             (quickly (get-operation replies-channel)))))))))
