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

(define-module (ares evaluation supervisor)
  #:use-module (ice-9 atomic)
  #:use-module (ares atomic)
  #:use-module (ares guile)
  #:use-module (ares evaluation)
  #:use-module (ares reusable-thread)
  #:use-module (ares alist)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module (fibers timers)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-197)
  #:export (get-or-create-evaluation-supervisor!
            evaluation-supervisor-thunk
            evaluation-supervisor-shutdown
            evaluation-supervisor-process-nrepl-message))

(define (replies-manager-thunk replies-channel reply!)
  (lambda ()
    (let loop ()
      ;; TODO: [Andrew Tropin, 2023-09-22] buffer those messages in
      ;; case reply blocks?
      (reply! (get-message replies-channel))
      (loop))))

(define* (evaluation-supervisor-thunk
          control-channel
          #:key
          (pure-dynamic-state (current-dynamic-state))
          ;; shutdown is graceful operation
          (shutdown-condition (make-condition))
          (finished-condition (make-condition)))

  (define (get-action command)
    (assoc-ref command 'action))

  ;; We can combine it with other blocking operation to extend the
  ;; possible sources of commands.  For example when thread evaluation
  ;; is finished, we can return a "run-evaluation" command.
  (define receive-command-operation
    (choice-operation
     (wrap-operation
      (wait-operation shutdown-condition)
      (lambda ()
        `((internal? . #t)
          (action . shutdown))))
     (wrap-operation
      (get-operation control-channel)
      (lambda (x)
        (alist-cons 'internal? #f x)))))

  (define (make-queue)
    '())

  (define (enqueue queue object)
    (cons object queue))

  (define (dequeue queue)
    (drop-right queue 1))

  (define (front queue)
    (last queue))

  (define (queue-empty? queue)
    (null? queue))

  (define (make-signaled-condition)
    (let ((condition (make-condition)))
      (signal-condition! condition)
      condition))

  (define evaluate-command-operation
    (wrap-operation
     (sleep-operation 0)
     (const `((internal? . #t)
              (action . evaluate)))))

  (define evaluation-thread-command-channel (make-channel))
  (define evaluation-thread-shutdown-condition (make-condition))

  (define (run-evaluation nrepl-message
                          get-next-command-operation
                          evaluation-queue)
    "Starts evaluation, return operation which unblocks on next command
arrival or when evaluation is finished, #t and rest of the queue."
    (let* ((finished-condition (make-condition))
           (replies-channel (make-channel))
           (command (front evaluation-queue))
           (reply! (assoc-ref command 'reply!)))
      (spawn-fiber
       (replies-manager-thunk replies-channel reply!))
      (put-message evaluation-thread-command-channel
                   `((action . evaluate)
                     (message . ,nrepl-message)
                     (replies-channel . ,replies-channel)
                     (evaluation-finished . ,finished-condition)))

      (list
       (choice-operation
        (wrap-operation
         (wait-operation finished-condition)
         (const `((internal? #t)
                  (action . finished))))
        get-next-command-operation)
       (or (alist-get-in '(message "id") command) "0")
       (dequeue evaluation-queue))))

  (define (spawn-reusable-thread ch)
    (with-dynamic-state pure-dynamic-state
      (lambda ()
        (make-reusable-thread ch))))

  (lambda ()
    (spawn-fiber (evaluation-thread-manager-thunk
                  evaluation-thread-command-channel
                  #:spawn-reusable-thread spawn-reusable-thread
                  #:shutdown-condition evaluation-thread-shutdown-condition))
    (let loop ((get-next-command-operation receive-command-operation)
               (evaluation-id #f)
               (evaluation-queue (make-queue)))
      (let* ((command (perform-operation get-next-command-operation))
             (action (assoc-ref command 'action))
             (repeat-loop (lambda ()
                            (loop get-next-command-operation
                                  evaluation-id
                                  evaluation-queue))))
        ;; (format #t "_________\ncommand: ~yevaluation-id: ~a\n\n"
        ;;         ;; get-next-command-operation
        ;;         command evaluation-id)
        (case action
          ((terminate)
           ;; (signal-condition! evaluation-id)
           ;; (signal-condition! evaluation-thread-terminate-condition)
           (signal-condition! finished-condition)
           'finished)

          ((shutdown)
           (signal-condition! evaluation-thread-shutdown-condition)
           (signal-condition! finished-condition)
           'finished)

          ((finished)
           (loop
            (if (queue-empty? evaluation-queue)
                receive-command-operation
                evaluate-command-operation)
            #f evaluation-queue))

          ((evaluate)
           ;; We can't be here if queue is empty
           (let* ((command (front evaluation-queue))
                  (nrepl-message (assoc-ref command 'message))
                  (reply! (assoc-ref command 'reply!)))
             (apply loop (run-evaluation
                          nrepl-message
                          receive-command-operation
                          ;; It's not obvious, that element is
                          ;; removed from the evaluation-queue
                          evaluation-queue))))

          ((process-nrepl-message)
           (let* ((message (assoc-ref command 'message))
                  (op (assoc-ref message "op")))
             (cond
              ((equal? "eval" op)
               (loop
                (if evaluation-id
                    get-next-command-operation
                    evaluate-command-operation)
                evaluation-id
                (enqueue evaluation-queue command)))

              ((equal? "interrupt" op)
               (if evaluation-id
                   ;; TODO: [Andrew Tropin, 2023-09-26] Add
                   ;; interrupt-id-mismatch.
                   ;; https://github.com/nrepl/nrepl/blob/master/src/clojure/nrepl/middleware/session.clj#L344
                   (begin
                     ((assoc-ref command 'reply!)
                      `(("status" . #("done"))))
                     (put-message evaluation-thread-command-channel
                                  `((action . interrupt))))
                   ((assoc-ref command 'reply!)
                    `(("status" . #("session-idle" "done")))))
               (loop
                get-next-command-operation
                evaluation-id
                evaluation-queue))

              ((equal? "stdin" op)
               (put-message evaluation-thread-command-channel
                            `((action . provide-input)
                              (stdin . ,(assoc-ref message "stdin"))))
               ((assoc-ref command 'reply!)
                `(("status" . #("done"))))
               (repeat-loop))

              (else
               (format (current-error-port)
                       "What is going on in evaluation supervisor: ~a\n"
                       message)
               (error "kawabanga"))))))))))

(define (evaluation-supervisor-shutdown control-channel)
  (put-message control-channel '((action . shutdown))))

(define (evaluation-supervisor-process-nrepl-message control-channel
                                                     message
                                                     reply!)
  (put-message control-channel `((action . process-nrepl-message)
                                 (message . ,message)
                                 (reply! . ,reply!))))
