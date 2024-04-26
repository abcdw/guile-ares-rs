(define-module (nonblocking-socket)
  #:export (create-socket))

(define* (create-socket
          #:key (port 1136))
  (define (make-default-socket family addr port)
    (let ((sock (socket PF_INET SOCK_STREAM 0)))
      ;; (fcntl sock F_SETFD FD_CLOEXEC)
      (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL)))
      sock))
  (define tmp-socket
    (make-default-socket AF_INET INADDR_LOOPBACK 1234))
  (connect tmp-socket (make-socket-address AF_INET INADDR_LOOPBACK port))
  (close-port tmp-socket))
