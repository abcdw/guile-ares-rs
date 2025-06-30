(define-module (ares evaluation test-utils)
  #:use-module (fibers operations)
  #:use-module (fibers timers)
  #:export (quickly))

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
