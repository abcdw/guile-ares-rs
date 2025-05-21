(define-module (nonblocking-socket)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (fibers)
  #:export (create-socket))

(define* (create-socket
          #:key (port 1134))
  (define (make-default-socket family addr port)
    (let ((sock (socket PF_INET SOCK_STREAM 0)))
      ;; (fcntl sock F_SETFD FD_CLOEXEC)
      (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL)))
      sock))
  (define tmp-socket
    (make-default-socket AF_INET INADDR_LOOPBACK 1234))
  (connect tmp-socket (make-socket-address AF_INET INADDR_LOOPBACK port))
  (close-port tmp-socket))

(define-public (tmp)
  (define prev-rw (current-read-waiter))
  (define prev-ww (current-write-waiter))
  (run-fibers
   (lambda ()
     (parameterize
         ((current-read-waiter prev-rw)
          (current-write-waiter prev-ww))
       (call-with-new-thread
        (lambda ()
          (create-socket))))
     (sleep 1)
     'yay)))
