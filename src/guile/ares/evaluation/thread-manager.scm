;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023, 2024 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2024 Nikita Domnitskii <nikita@domnitskii.me>
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

;;; Commentary:

;;; The code in this module is for the thread manager thread. The
;;; thread manager is an asynchronous interface to the evaluation
;;; thread.
;;;
;;; The nrepl channel receives messages from the evaluation and
;;; extended-evaluation nREPL operations. Messages are passed to the
;;; evaluation thread through the thread channel. Evaluations are
;;; queued in case the evaluation thread is already evaluating.
;;;
;;; Each request to the thread channel is paired with a reply callback
;;; to match each reply with the right nREPL request.
;;;
;;; The stdin channel is used to communicate standard input.

;;; Code:

(define-module (ares evaluation thread-manager)
  #:use-module (ares reusable-thread)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (ares evaluation io)
  #:use-module (ares evaluation eval)
  #:use-module (ares evaluation serialization)
  #:use-module (ares ports)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module (ice-9 and-let-star)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 q)
  #:export (evaluation-thread-manager-thunk
            evaluation-thread-manager))

(define* (evaluation-thread-manager-thunk
          command-channel
          #:key
          (spawn-reusable-thread make-reusable-thread)
          ;; TODO: [Andrew Tropin, 2023-10-16] Implement shutdown
          (shutdown-condition (make-condition))
          (terminate-condition (make-condition)))
  "Spawn a thread and can run/interrupt evaluation on it via commands sent to
COMMAND-CHANNEL."

  (define (open-channel-input-port request-channel input-channel)
    (define buffer "")
    (define position 0)
    (define (length)
      (string-length buffer))
    ;; https://tonsky.me/blog/unicode/
    (define (code-points-left)
      (- (length) position))

    (define (read! dst start count)
      (when (= 0 (code-points-left))
        (put-message request-channel `((action . need-input)))
        (set! buffer (get-message input-channel))
        (set! position 0))
      (let ((count (min count (code-points-left))))
        (string-copy! dst start buffer position (+ position count))
        (set! position (+ position count))
        count))

    (make-custom-textual-input-port "channel-port" read! #f #f #f))

  (lambda ()
    (call-with-pipes
     (unbuffer-pipes! (make-pipes 2))
     (lambda (pipes)
       (let* ((result-channel (make-channel))
              (input-request-channel (make-channel))
              (stdin-channel (make-channel))
              (input-port (open-channel-input-port
                           input-request-channel
                           stdin-channel))
              (evaluation-rethread (spawn-reusable-thread result-channel)))
         (let loop ((reply-channel #f)
                    (evaluation-finished #f)
                    (output-finished-condition #f))
           (define (repeat-loop)
             (loop reply-channel evaluation-finished output-finished-condition))
           (define (reset-loop)
             (loop #f #f #f))

           (define (mark-evaluation-finished!)
             ;; We can't directly signal EVALUATION-THUNK-FINISHED
             ;; inside evaluation thread, because call/cc can restore
             ;; context, where it is not available, so we need to rely
             ;; on RESULT-CHANNEL for understanding if evaluation
             ;; actually finished.
             (signal-condition! evaluation-finished)
             (wait output-finished-condition))

           (define (reply-with-messages messages)
             (for-each
              (lambda (m)
                (put-message reply-channel m))
              messages))

           (define command
             (perform-operation
              (choice-operation
               (get-operation input-request-channel)
               (get-operation result-channel)
               (get-operation command-channel))))

           (define action (assoc-ref command 'action))

           ;; (format #t "command=> ~y" command)
           (case action
             ((return-value)
              (let ((value (assoc-ref command 'value)))
                (mark-evaluation-finished!)
                ;; TODO: [Andrew Tropin, 2023-10-18] Add pretty print
                ;; for returning value
                (reply-with-messages (evaluation-result->nrepl-messages value))
                (reset-loop)))

             ((evaluate)
              (let ((nrepl-message (assoc-ref command 'message))
                    (replies-channel (assoc-ref command 'replies-channel))
                    (wrap-with-ports-thunk-channel (make-channel))
                    (output-finished-condition (make-condition))
                    (evaluation-thunk-finished
                     (assoc-ref command 'evaluation-finished)))

                (or evaluation-thunk-finished
                    (error "evaluation-finished is not provided"))
                (spawn-fiber
                 (setup-redirects-for-ports-thunk
                  pipes
                  input-port
                  evaluation-thunk-finished
                  replies-channel
                  wrap-with-ports-thunk-channel
                  output-finished-condition))

                (reusable-thread-discard-and-run
                 evaluation-rethread
                 ((get-message wrap-with-ports-thunk-channel)
                  (evaluation-thunk nrepl-message)))

                (loop replies-channel evaluation-thunk-finished
                      output-finished-condition)))
             ((interrupt)
              (reply-with-messages
               (interrupt-result->nrepl-messages
                (car
                 (reusable-thread-interrupt evaluation-rethread))))
              (mark-evaluation-finished!)
              (reset-loop))
             ((need-input)
              (reply-with-messages
               `((("status" . #("need-input")))))
              (repeat-loop))
             ((provide-input)
              (put-message stdin-channel (assoc-ref command 'stdin))
              (repeat-loop))
             (else
              (error "it sholud not get here")))))))))

(define (evaluation-thread-manager nrepl-channel)
  "Spawn a thread and can run/interrupt evaluation on it via commands sent to
COMMAND-CHANNEL."
  (define thread-channel (make-channel))
  (define stdin-channel (make-channel))
  (define reset-evaluation #f)          ;use only in evaluation thread
  (define thread
    (call-with-new-thread
     (lambda ()
       (call/cc
        (lambda (cont)
          (set! reset-evaluation cont)))
       (evaluation-loop
        thread-channel
        #:stdin-channel stdin-channel))))
  (define actions (make-q))
  ;; If we get two interruption requests in quick succession, the
  ;; previous async-mark can still be in queue, therefore
  ;; system-async-mark will do nothing and the user will get no
  ;; reply. We use this variable to check for that case and send a
  ;; reply.
  (define waiting-for-async-mark? (make-atomic-box #f))

  (define (eval-callback reply!)
    "Returns a lambda that receives a message from the evaluation loop and
sends it as an nREPL message to REPLY!."
    (lambda (command)
      (let ((type (car command)))
        (case type
          ((need-input)
           (reply! '(("status" . #("need-input")))))
          ((result)
           (for-each reply! (evaluation-result->nrepl-messages (cadr command))))
          ((output)
           (reply! `(("out" . ,(cadr command)))))
          ((error)
           (reply! `(("err" . ,(cadr command)))))
          (else (error "unknown thread command"))))))

  (define (enq-action message reply)
    (enq! actions `((reply . ,reply)
                    (action . ,message))))

  (while #t
    (let* ((message
            (perform-operation
             (apply choice-operation
                    (get-operation nrepl-channel)
                    (if (q-empty? actions)
                        '()
                        (list
                         (wrap-operation (put-operation thread-channel (q-front actions))
                                         (lambda ()
                                           (deq! actions)
                                           (continue))))))))
           (nrepl-reply! (assoc-ref message "reply!"))
           (eval-reply (eval-callback nrepl-reply!))
           (op (assoc-ref message "op")))
      (match op
        ("eval"
         (enq-action `(evaluate ,message) eval-reply))
        ("stdin"
         (put-message stdin-channel (assoc-ref message "stdin"))
         (nrepl-reply! `(("status" . #("done")))))
        ("interrupt"
         (if (atomic-box-ref waiting-for-async-mark?)
             (nrepl-reply! `(("status" . #("done" "interrupting"))))
             (system-async-mark
              (lambda ()
                (atomic-box-set! waiting-for-async-mark? #f)
                (let ((state (fluid-ref evaluation-state)))
                  (fluid-set! evaluation-state 'interrupting)
                  (case state
                    ((idle)
                     (nrepl-reply! `(("status" . #("done" "session-idle")))))
                    ((evaluation)
                     (abort-to-prompt
                      evaluation-tag
                      eval-reply))
                    ((interrupting)
                     (nrepl-reply! `(("status" . #("done" "interrupting")))))
                    (else
                     (nrepl-reply! `(("status" . #("done" "error"))
                                     ("error" . "unknown evaluation state")))
                     (error "unknown evaluation state")))))
                thread)))
        (_
         (format (current-error-port)
                 "unknown nREPL action ~a~%" op))))))
