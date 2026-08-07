(define-module (ares evaluation test-utils)
  #:use-module (fibers operations)
  #:use-module (fibers timers)
  #:export (quickly
            valid-stack?))

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

(define (valid-source? source)
  (and (list? source)
       (assq 'line source)
       (assq 'column source)
       (let ((file (assq 'file source)))
         (or (not file) (cdr file)))))

(define (valid-stack? stack)
  (if (null? stack)
      #t
      (and
       (car stack)
       (assq 'procedure-name (car stack))
       (assq 'arguments (car stack))
       (assq 'environment (car stack))
       (let ((source (assq 'source (car stack))))
         (or (not source)
             (valid-source? (cdr source))))
       (valid-stack? (cdr stack)))))
