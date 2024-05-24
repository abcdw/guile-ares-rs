(define-module (nrepl server)
  #:use-module ((nrepl bootstrap) #:prefix bootstrap:)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (ice-9 match)
  #:export (run-nrepl-server))


;;;
;;; loop
;;;

(define (socket-loop socket addr initial-context)
  (let loop ()
    (match (accept socket SOCK_NONBLOCK)
      ((client . addr)
       (spawn-fiber
        (lambda ()
          (setvbuf client 'block 1024)
          ;; Disable Nagle's algorithm.  We buffer ourselves.
          (setsockopt client IPPROTO_TCP TCP_NODELAY 1)
          (bootstrap:nrepl-loop
           (bootstrap:add-ports initial-context client client))))
       (loop)))))

(define (make-default-socket family addr port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (fcntl sock F_SETFD FD_CLOEXEC)
    (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL)))
    (bind sock family addr port)
    sock))

(define* (listen-socket socket addr)
  (listen socket 1024)
  (sigaction SIGPIPE SIG_IGN))

(define* (run-nrepl-server
          #:key
          (host #f)
          (family AF_INET)
          (addr (if host (inet-pton family host) INADDR_LOOPBACK))
          (port 7888)
          (started? (make-condition)))

  (define socket (make-default-socket family addr port))
  (define host (gethostbyaddr addr))
  (define hostname (hostent:name host))
  (listen-socket socket addr)
  (format #t "nREPL server started on port ~a on host ~a - nrepl://~a:~a\n"
          port hostname hostname port)
  (signal-condition! started?)

  (define initial-context (bootstrap:make-initial-context))

  (dynamic-wind
    (lambda () 'hi)
    (lambda ()
      (run-fibers
       (lambda ()
         (socket-loop socket addr initial-context))
       #:drain? #t))
    (lambda ()
      (false-if-exception (close-port socket)))))


;; dynamic-wind solution for closing socket
;; https://git.savannah.gnu.org/cgit/guix.git/tree/guix/scripts/repl.scm?h=a831efb52f49a8424a915f30486730e0fd4ba4e2#n139

;; (use-modules (ice-9 threads))

;; (define server-thread #f)

;; (define (restart-server-thread)
;;   (when (thread? server-thread)
;;     (cancel-thread server-thread))

;;   (set! server-thread (call-with-new-thread run-nrepl-server)))

;; (restart-server-thread)

;; server-thread
;; (cancel-thread server-thread)
;; (join-thread server-thread 1)

