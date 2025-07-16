;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
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

(define-module (ares evaluation io)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module (fibers channels)
  #:use-module (fibers io-wakeup)
  #:use-module (fibers)
  #:use-module (ares ports)
  #:export (setup-redirects-for-ports-thunk
            output-stream-manager-thunk
            open-callback-input-port
            open-callback-output-port
            call-with-current-ports))

;; Do channel accepts nrepl messages or just strings?  Strings can
;; help to delay nrepl related logic further up the stack.  But will
;; require additional actor to wrap messages for each output port or
;; one aggregating actor with non-trivial synchronization logic.
(define* (output-stream-manager-thunk input-port
                                      wrap-function
                                      replies-channel
                                      process-finished-condition
                                      #:key
                                      (finished-condition (make-condition)))
  "Watches INPUT-PORT and when something arrives reads it as a string,
wraps with WRAP-FUNCTION and sends to the REPLIES-CHANNEL.  Works
until PROCESS-FINISHED-CONDITION is signaled or INPUT-PORT is closed.
Signals FINISHED-CONDITION, when it is completed."
  (define (port-open? port) (not (port-closed? port)))
  (lambda ()
    (let loop ()
      (let ((op-value
             (perform-operation
              (choice-operation
               (wrap-operation
                (wait-until-port-readable-operation input-port)
                (const 'ready))
               (wrap-operation
                (wait-operation process-finished-condition)
                (const 'finished))))))

        ;; Try to read anyway, in case something came before process finished
        (when (and (port-open? input-port) (char-ready? input-port))
          (put-message replies-channel
                       (wrap-function (read-all-chars-as-string input-port))))

        ;; It doesn't make sense to keep watching port if it's already closed
        (if (and (equal? 'ready op-value) (port-open? input-port))
            (loop)
            (signal-condition! finished-condition))))))

(define (setup-redirects-for-ports-thunk output-pipes
                                         input-port
                                         thunk-finished-condition
                                         replies-channel
                                         wrap-with-ports-channel
                                         finished-condition)
  "Returns a thunk, which setups redirects for ports, spawning respective
output stream managers and wait until they finished.  After everything
is set, puts a wrap-with-ports function into WRAP-WITH-PORTS-CHANNEL.
Stream managers waits until THUNK-FINISHED is signalled."

  (define (wrap-output-with tag)
    "Return a function, which wraps argument into alist."
    (lambda (v) `((,tag . ,v))))

  (lambda ()
    (match output-pipes
      ;; Destructure a list of 2 pipes into 4 separate variables
      (((stdout-input-port . stdout-output-port)
        (stderr-input-port . stderr-output-port))
       (let ((wrap-with-ports (lambda (thunk)
                                (lambda ()
                                  (with-current-ports
                                   stdout-output-port
                                   stderr-output-port
                                   input-port
                                   thunk))))
             (stdout-finished (make-condition))
             (stderr-finished (make-condition)))
         ;; TODO: [Andrew Tropin, 2023-09-06] Add input-stream-manager
         ;; use custom or soft ports?

         (spawn-fiber
          (output-stream-manager-thunk stdout-input-port
                                       (wrap-output-with "out")
                                       replies-channel
                                       thunk-finished-condition
                                       #:finished-condition stdout-finished))
         (spawn-fiber
          (output-stream-manager-thunk stderr-input-port
                                       (wrap-output-with "err")
                                       replies-channel
                                       thunk-finished-condition
                                       #:finished-condition stderr-finished))

         (put-message wrap-with-ports-channel wrap-with-ports)
         (wait stdout-finished)
         (wait stderr-finished)
         (signal-condition! finished-condition))))))

(define (open-callback-input-port input-channel request-callback)
    (define buffer "")
    (define position 0)
    (define (length)
      (string-length buffer))
    ;; https://tonsky.me/blog/unicode/
    (define (code-points-left)
      (- (length) position))

    (define (read! dst start count)
      (when (= 0 (code-points-left))
        (request-callback)
        (set! buffer (get-message input-channel))
        (set! position 0))
      (let ((count (min count (code-points-left))))
        (string-copy! dst start buffer position (+ position count))
        (set! position (+ position count))
        count))

    (make-custom-textual-input-port "channel-input-port" read! #f #f #f))

(define (open-callback-output-port output-callback)
  (define (write! str start count)
    (output-callback str)
    count)

  (make-custom-textual-output-port "channel-output-port" write! #f #f #f))

(define* (call-with-current-ports thunk
                                  #:key
                                  (input (current-input-port))
                                  (output (current-output-port))
                                  (error (current-error-port))
                                  (warning (current-warning-port)))
  (define value
    (parameterize ((current-input-port input)
                   (current-output-port output)
                   (current-error-port error)
                   (current-warning-port warning))
      (thunk)))
  (define (flush port)
    (unless (port-closed? port)
      (force-output port)))
  (flush output)
  (flush error)
  (flush warning)
  value)
