;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
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

(define-module (ares reusable-thread)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (nrepl atomic)
  #:use-module (srfi srfi-9)
  #:export (reusable-thread
            reusable-thread-thread
            reusable-thread-get-value
            reusable-thread-nutex
            reusable-thread-discard-and-run
            reusable-thread-interrupt
            reusable-thread-shutdown
            make-reusable-thread))

;;;
;;; Eval Thread
;;;

(define-record-type <reusable-thread>
  (%make-reusable-thread thread nutex result-channel)
  reusable-thread?
  (thread reusable-thread-thread)
  (nutex reusable-thread-nutex)
  (result-channel reusable-thread-result-channel))

(define (make-nutex)
  "A syncronization point, similiar to unowned mutex, but it created
already locked and works without suspending the whole thread."
  (make-atomic-box (make-condition)))

(define (nutex-lock! nutex)
  (define new-condition (make-condition))
  (define (update-condition old-condition)
    (wait old-condition)
    new-condition)
  (atomic-box-update! nutex update-condition))

(define (nutex-unlock! nutex)
  (signal-condition! (atomic-box-ref nutex)))

(define (nutex-wait nutex)
  (wait (atomic-box-ref nutex)))

(define (rthread-lock-or-get-value! rthread)
  (define nutex (reusable-thread-nutex rthread))
  (define result-channel (reusable-thread-result-channel rthread))
  (define new-condition (make-condition))

  (define (value-or-new-condition old-condition)
    (perform-operation
     (choice-operation
      (wrap-operation
       (wait-operation old-condition)
       (const new-condition))
      (get-operation result-channel))))

  (let loop ((old-value (atomic-box-ref nutex)))
    (define new-value (value-or-new-condition old-value))
    (if (eq? new-condition new-value)
        (let ((cas-value
               (atomic-box-compare-and-swap! nutex old-value new-value)))
          (if (eq? old-value cas-value)
              new-value ; lock acquired
              (loop cas-value)))

        ;; Value returned
        new-value)))

(define thread-entry-point-tag (make-prompt-tag "thread-entry-point"))

(define (command-loop-thunk nutex result-channel)
  (lambda ()

    (define (suspending-thunk)
      (define seconds-in-day 86400)
      (let loop ()
        (sleep seconds-in-day)
        (loop)))

    (define (return-value value)
      (put-message result-channel value))
    (let loop ((thunk #f))
      (call-with-prompt
       thread-entry-point-tag
       (lambda ()
         (nutex-unlock! nutex)
         (define thunk-value ((or thunk suspending-thunk)))
         ;; it either locked or the thread is interrupted
         (nutex-lock! nutex)
         (abort-to-prompt
          thread-entry-point-tag
          `((action . return-value)
            (value . ,thunk-value))))

       (lambda (k command)
         (define action (assoc-ref command 'action))

         (case action
           ((return-value)
            (return-value command))
           ((interrupt)
            (return-value `((action . interrupt)
                            (status . ,(if thunk 'done 'idle))))))

         (case action
           ((run)
            (loop (assoc-ref command 'thunk)))
           ((shutdown) 'finished)
           (else (loop #f))))))))

(define* (make-reusable-thread #:optional (result-channel (make-channel)))
  "Starts a thread and makes sure it entered restart prompt."
  (let* ((nutex (make-nutex))
         (thread
          (call-with-new-thread (command-loop-thunk nutex result-channel))))
    (nutex-wait nutex)
    (%make-reusable-thread thread nutex result-channel)))

(define (%reusable-thread-do reusable-thread thunk)
  "Schedule a thunk for execution on REUSABLE-THREAD.  This a helper
function, don't use it directly, it doesn't guarantee that nutex will
be unlocked."
  (let loop ((replies '()))
    (define lock-or-value (rthread-lock-or-get-value! reusable-thread))
    (if (condition? lock-or-value)
        (begin
          (system-async-mark
           thunk
           (reusable-thread-thread reusable-thread))
          (reverse replies))
        (loop (cons lock-or-value replies)))))

(define (reusable-thread-get-value-operation reusable-thread)
  (get-operation (reusable-thread-result-channel reusable-thread)))

(define (reusable-thread-get-value reusable-thread)
  (perform-operation (reusable-thread-get-value-operation reusable-thread)))

(define (command->thunk command)
  (lambda ()
    (abort-to-prompt thread-entry-point-tag command)))

(define (%reusable-thread-schedule-command reusable-thread command)
  (%reusable-thread-do reusable-thread (command->thunk command)))

(define* (reusable-thread-discard-and-run reusable-thread thunk)
  "Interrupt and reuse REUSABLE-THREAD with a new THUNK.  It pauses all
current computations, starts a THUNK and after THUNK is finished
aborts to thread entry point tag, which cancels all previous
computations."
  (%reusable-thread-schedule-command
   reusable-thread
   `((action . run)
     (thunk . ,thunk))))

(define* (reusable-thread-shutdown reusable-thread)
  (%reusable-thread-schedule-command
   reusable-thread
   `((action . shutdown))))

(define* (reusable-thread-interrupt reusable-thread)
  (append
   (%reusable-thread-schedule-command
    reusable-thread
    `((action . interrupt)))
   (list (reusable-thread-get-value reusable-thread))))
