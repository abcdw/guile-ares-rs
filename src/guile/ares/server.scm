(define-module (ares server)
  #:use-module ((ares loop) #:prefix ares.loop:)
  #:use-module ((nrepl bootstrap) #:prefix nrepl.bootstrap:)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (ice-9 match)
  #:export (run-nrepl-server))


;;;
;;; Socket fiber-based server
;;;

(define (socket-loop socket addr initial-context)
  "Adds input and output ports to @code{initial-context} and starts Ares
loop inside a separate fiber."
  (let loop ()
    (match (accept socket SOCK_NONBLOCK)
      ((client . addr)
       (spawn-fiber
        (lambda ()
          (setvbuf client 'block 1024)
          ;; Disable Nagle's algorithm.  We buffer ourselves.
          (setsockopt client IPPROTO_TCP TCP_NODELAY 1)
          (ares.loop:loop
           (ares.loop:add-ports initial-context client client))))
       (loop)))))

(define (make-default-socket family addr port)
  "Creates a non-blocking server socket."
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (fcntl sock F_SETFD FD_CLOEXEC)
    (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL)))
    (bind sock family addr port)
    sock))

(define (listen-socket! socket)
  "Make socket listening for incomming connections.  Simply a wrapper for
low-level functions."
  ;; Start accepting connections on socket and set a connection
  ;; backlog size to 1024
  (listen socket 1024)
  ;; Set handler for situations, when client interrupted connection
  (sigaction SIGPIPE SIG_IGN))

(define* (run-nrepl-server
          #:key
          (host #f)
          (family AF_INET)
          (addr (if host (inet-pton family host) INADDR_LOOPBACK))
          (port 7888)
          (started? (make-condition)))
  "Runs nREPL server to listen on @code{host}:@code{port}, prints a
greeting and signals @code{started?}, when socket it starts to listen
the socket.

It creates a fibers environment and handles all incoming connection
on separate fibers."

  (define socket (make-default-socket family addr port))
  (define host (gethostbyaddr addr))
  (define hostname (hostent:name host))
  (listen-socket! socket)
  (format #t "nREPL server started on port ~a on host ~a - nrepl://~a:~a\n"
          port hostname hostname port)
  (signal-condition! started?)

  ;; We initialize context before run-fibers to capture clean current
  ;; dynamic state, also we share ares/state part of the context for
  ;; all clients, but input/output ports are unique for each one.
  (define initial-context
    (ares.loop:make-initial-context nrepl.bootstrap:bootstrap-extensions))

  (run-fibers
   (lambda ()
     (socket-loop socket addr initial-context))
   #:drain? #t)
  (false-if-exception (close-port socket)))


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

