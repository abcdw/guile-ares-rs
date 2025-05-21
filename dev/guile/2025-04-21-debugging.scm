(define-module (2025-04-21-debugging))

;; (use-modules (ice-9 threads))
;; (define th
;;   (make-thread
;;    (lambda ()
;;      (let loop ()
;;        (format #t "yept: ~a\n"
;;                (exact->inexact (/ (get-internal-run-time) internal-time-units-per-second)))
;;        (loop)))))

;; (cancel-thread th)
;; (thread-exited? th)

;; (list (exact->inexact (/ (get-internal-run-time) internal-time-units-per-second))
;;       (exact->inexact (/ (get-internal-real-time) internal-time-units-per-second)))


;; (define st #f)
;; (let ((a 10)
;;       (tmp (lambda ()
;;              "the function"
;;              (set! st (make-stack #t))
;;              34)))
;;   (tmp)
;;   a)

;; (display-backtrace st (current-output-port))
(define (call-with-backtrace thunk)
  (with-exception-handler
    (lambda (exn)
      (backtrace)
      (raise-exception exn))
    thunk))



(use-modules (ice-9 control))
(define (call-with-backtrace thunk)
  (let/ec cancel
          (with-exception-handler
           (lambda (exn)
             (backtrace)
             (display exn)
             (cancel #f))
           (lambda ()
             (start-stack 'clean-stack
              (thunk))))))

;; (call-with-backtrace (lambda () (/ 0 0)))

(use-modules (system vm trap-state)
             (system vm trace)
             (system vm traps)
             (system vm vm))

(define (fact1 n)
  (if (zero? n) 1
      (* n (fact1 (1- n)))))

(define (comment)
  (begin
    (trap-at-procedure-call fact1 (lambda () (format #t "entered\n")))
    (vm-trace-level)
    (list-traps)
    (the-trap-state))
  (call-with-trace (lambda () (fact1 5) (list-traps)))

  (set-vm-trace-level! 1)
  (vm-trace-level))
