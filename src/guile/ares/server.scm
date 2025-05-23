(define-module (ares server)
  #:use-module ((ares loop) #:prefix ares.loop:)
  #:use-module ((ares nrepl bootstrap) #:prefix nrepl.bootstrap:)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (ice-9 match)
  #:export (run-nrepl-server))


;;;
;;; Socket fiber-based server
;;;

(define (make-client client id thunk)
  (spawn-fiber thunk))

(define (socket-loop socket addr initial-context on-connection)
  "Adds input and output ports to @code{initial-context} and starts Ares
loop inside a separate fiber."
  (let loop ((id 1))
    (match (accept socket SOCK_NONBLOCK)
      ((client . addr)
       (on-connection
	client id
	(lambda ()
	  (setvbuf client 'block 1024)
	  ;; Disable Nagle's algorithm.  We buffer ourselves.
	  (setsockopt client IPPROTO_TCP TCP_NODELAY 1)
	  (ares.loop:loop
	   (ares.loop:add-ports initial-context client client))))
       (loop (1+ id))))))

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

(define (generate-random-port)
  "Generates random ephemeral port number.  Ports above 49151 are not
registered for any specific applications"
  (+ 49152 (random 16384 (random-state-from-platform))))

(define* (run-nrepl-server
          #:key
          (host #f)
          (family AF_INET)
          (addr (if host (inet-pton family host) INADDR_LOOPBACK))
          (port (generate-random-port))
          (started? (make-condition))
          (nrepl-port-file? #t)
          (nrepl-port-path ".nrepl-port")
	  (standalone? #t)
	  (on-connection make-client))
  "Runs nREPL server to listen on @code{host}:@code{port}, prints a
greeting and signals @code{started?}, when socket it starts to listen
the socket.

Creates @code{nrepl-port-path} with port number if
@code{nrepl-port-file?} is @code{#t}.

If @code{standalone?} is @code{#t}, it creates a fibers environment
and runs until the server is closed. Otherwise, it will run in the
current fibers scheduler and returns a procedure that can be called to
stop the server.

For every connection, @var{on-connection} is called with a socket,
client id and a thunk that should be called to start the nREPL
connection.  By default it runs thunk inside a new fiber."

  (define socket (make-default-socket family addr port))
  (define host (gethostbyaddr addr))
  (define hostname (hostent:name host))
  (listen-socket! socket)
  (format #t "nREPL server started on port ~a on host ~a - nrepl://~a:~a\n"
          port hostname hostname port)

  (when nrepl-port-file?
    (call-with-output-file nrepl-port-path
      (lambda (p)
        (format p "~a\n" port))))

  (signal-condition! started?)

  ;; We initialize context before run-fibers to capture clean current
  ;; dynamic state, also we share ares/state part of the context for
  ;; all clients, but input/output ports are unique for each one.
  (define initial-context
    (ares.loop:make-initial-context
     (append
      (list
       ;; TODO: [Andrew Tropin, 2025-05-10] Make it less verbose or
       ;; even silent out of the box, printing completions or other
       ;; long lists to the terminal have negative impact on
       ;; perfomance and thus user experience.  Provide operations and
       ;; client-side UI/configuration options for controlling log
       ;; verbosness.
       (@ (ares-extension ares logging) ares.logging)
       ;; Can be loaded on-demand (we can do it in the future if it
       ;; make sense perfomance wise)
       (@ (ares-extension ares guile macroexpansion) ares.guile.macroexpansion))
      nrepl.bootstrap:bootstrap-extensions)))

  (if standalone?
      (begin
	;; TODO: It would be nice if this also returned a lambda to
	;; close the server. Shutdown behaviour would be customized by
	;; arguments to the lambda.
        (run-fibers
	 (lambda ()
	   (socket-loop socket addr initial-context on-connection))
	 #:drain? #t)
	(false-if-exception (close-port socket)))
      (begin
	(spawn-fiber
	 (lambda ()
	   (socket-loop socket addr initial-context on-connection)))
	(lambda ()
	  (close-port socket)))))


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

