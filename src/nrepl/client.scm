(define-module (nrepl client)
  #:use-module (bencode)
  #:use-module (fibers)
  #:export (make-nrepl-client-socket))

(define* (make-nrepl-client-socket #:key (port 7888))
  (let ((socket (socket PF_INET SOCK_STREAM 0)))
    (connect socket AF_INET (inet-pton AF_INET "127.0.0.1") port)
    socket))
