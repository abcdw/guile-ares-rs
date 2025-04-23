(define-module (2025-04-23-backtrace))

(define (call-with-backtrace thunk)
  (with-exception-handler
    (lambda (exn)
      (backtrace)
      (raise-exception exn))
    thunk))

(define (boom)
  (call-with-backtrace (lambda () (/ 0 0))))

;; (boom)
