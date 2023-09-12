(define-module (nrepl server)
  #:duplicates (replace last)
  #:use-module (bencode)
  #:use-module (scheme base)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 atomic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-197)
  #:use-module (uuid)
  #:export (run-nrepl-server))

(define (simple-log . args)
  ""
  (define (repeat str n)
    (string-join (map (lambda (x) str) (iota n)) " "))
  (if (not (string? (car args)))
      (apply format (current-error-port) (repeat "~s" (length args)) args)
      (apply format (current-error-port) args))
  (newline))

(define log (make-parameter simple-log))


;;;
;;; REPL
;;;

(define (self-quoting? x)
  "Return #t if X is self-quoting."
  (letrec-syntax ((one-of (syntax-rules ()
                            ((_) #f)
                            ((_ pred rest ...)
                             (or (pred x)
                                 (one-of rest ...))))))
    (one-of symbol? string? keyword? pair? null? array?
            number? boolean? char?)))

(define (value->sexp value)
    (if (self-quoting? value)
        `(value ,value)
        `(non-self-quoting ,(object-address value)
                           ,(object->string value))))

(define (stack->frames stack)
  "Return STACK's frames as a list."
  (unfold (cute >= <> (stack-length stack))
          (cut stack-ref stack <>)
          1+
          0))

(define (frame->sexp frame)
  `(,(frame-procedure-name frame)
    ,(match (frame-source frame)
       ((_ (? string? file) (? integer? line) . (? integer? column))
        (list file line column))
       (_
        '(#f #f #f)))))

(define (nrepl-prompt-message)
  "scheme@(module-here-someday)> ")

(define (eval-expression expression)
  (define (handle-exception key . args)
    (define reply
      (let ((stack (make-stack #t)
                   ;; (if (nrepl-prompt)
                   ;;     (make-stack #t handle-exception (nrepl-prompt))
                   ;;     (make-stack #t))
                   ))
        ;; Note: 'make-stack' returns #f if there's no 'handle-exception'
        ;; stack frame, which is the case when this file is being
        ;; interpreted as with 'primitive-load'.
        `(exception (arguments ,key ,@(map value->sexp args))
                    (stack ,@(map frame->sexp
                                  (if stack
                                      (stack->frames stack)
                                      '()))))))
    ((log) "repl result: ~s" reply)
    reply)

  ;; TODO: Rewrite to with-exception-handler
  (catch #t
    (lambda () (primitive-eval expression))
    ;; TODO: Return a real exception
    (const "exception happened")
    handle-exception))

(define (read-expression code)
  (catch #t
    (lambda () (with-input-from-string code read))
    ;; TODO: Return a real exception and handle it properly
    (const "Couldn't read the code")))


;;;
;;; Operations
;;;

(define (response-for input msg)
  (let ((id (assoc-ref input "id"))
        (session (assoc-ref input "session")))
    (chain msg
           (acons "id" (or id "unknown") _)
           (acons "session" (or session "none") _))))

(define (eval-op sessions input)
  (let* ((code (assoc-ref input "code"))
         (result (receive (. vals)
                     (eval-expression
                      (read-expression code))
                   ;; TODO: handle multiple return values
                   (car vals)))
         (value (format #f "~s" result))
         (response `(("status" . #("done"))
                     ("ns" . "user")
                     ("value" . ,value))))
    (response-for input response)))

(define (atomic-box-update! box proc)
  "Atomically updates value of BOX to (PROC BOX-VALUE), returns new value.
PROC may be called multiple times, and thus PROC should be free of
side effects."
  (let loop ()
    (let* ((old-value (atomic-box-ref box))
           (new-value (proc old-value)))
      (if (eq? old-value (atomic-box-compare-and-swap! box old-value new-value))
          new-value
          (loop)))))

(define (register-session! sessions session-id new-session)
  (atomic-box-update!
   sessions
   (lambda (s) (acons session-id new-session s))))

(define (get-session-ids sessions)
  "Returns a vector of session ids suitable for bencoding."
  (list->vector (map car (atomic-box-ref sessions))))

(define (clone-op sessions input)
  (let ((new-session-id (uuid))
        (new-session #f))
    (register-session! sessions new-session-id new-session)
    (response-for
     input
     `(("status" . #("done"))
       ("new-session" . ,new-session-id)))))

(define (completions-op sessions input)
  (let* ((id (assoc-ref input "id"))
         (response `(("id" . ,id)
                     ("completions" . #()))))
    response))

(define (ls-sessions-op sessions input)
  (response-for
   input
   `(("status" . #("done"))
     ("sessions" . ,(get-session-ids sessions)))))

;; interrupt
;; Attempts to interrupt some executing request. When interruption succeeds, the thread used for execution is killed, and a new thread spawned for the session. While the session middleware ensures that Clojure dynamic bindings are preserved, other ThreadLocals are not. Hence, when running code intimately tied to the current thread identity, it is best to avoid interruptions.

;; Required parameters
;; :session The ID of the session used to start the request to be interrupted.

;; Optional parameters
;; :interrupt-id The opaque message ID sent with the request to be interrupted.

;; Returns
;; :status 'interrupted' if a request was identified and interruption will be attempted 'session-idle' if the session is not currently executing any request 'interrupt-id-mismatch' if the session is currently executing a request sent using a different ID than specified by the "interrupt-id" value 'session-ephemeral' if the session is an ephemeral session

(define (interrupt-op sessions input)
  ((log) "~a" sessions)
  (response-for
   input
   '("status" . "session-idle")))

(define (describe-op sessions input)
  `(lol))

(define default-operations
  `(("eval" . ,eval-op)
    ("describe" . ,describe-op)
    ("ls-sessions" . ,ls-sessions-op)
    ("interrupt" . ,interrupt-op)
    ("completions" . ,completions-op)
    ("clone" . ,clone-op)))

(define (get-operation operations op)
  (assoc-ref operations op))

(define (run-operation sessions operations input)
  ((log) "input: ~s" input)
  (let* ((op (assoc-ref input "op"))
         (operation (get-operation operations op)))
    (if operation
        (operation sessions input)
        "no-such-operation")))


;;;
;;; loop
;;;

(define (process-request sessions client input)
  ;; Make process request asyncronous
  (spawn-fiber
   (lambda ()
     (let ((result (if input
                       (run-operation sessions default-operations input)
                       #f)))
       ((log) "response: ~s" result)

       (scm->bencode result client))
     (force-output client))))

(define* (client-loop client addr sessions)
  ((log) "new connection: ~a" client)
  ;; ((log) (port-filename client) (port->fdes client))

  (let loop ()
    (if (eof-object? (peek-u8 client))
        (begin
          ((log) "closing connection: ~a" client)
          ;; Don't close until all session are finished ????
          ;; (cleanup-sessions! client)
          (close-port client))
        (let ((input (guard (ex (else #f)) (bencode->scm client))))
          (unless input ((log) "input is malformed"))
          (process-request sessions client input)
          (loop)))))

(define (socket-loop socket addr sessions)
  (let loop ()
    (match (accept socket SOCK_NONBLOCK)
      ((client . addr)
       (spawn-fiber
        (lambda ()
          (setvbuf client 'block 1024)
          ;; Disable Nagle's algorithm.  We buffer ourselves.
          (setsockopt client IPPROTO_TCP TCP_NODELAY 1)

          (client-loop client addr sessions)))
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
          (started? (make-condition))
          (log-function simple-log))

  (define socket (make-default-socket family addr port))
  (define host (gethostbyaddr addr))
  (define hostname (hostent:name host))
  (listen-socket socket addr)
  (format #t "nREPL server started on port ~a on host ~a - nrepl://~a:~a\n"
          port hostname hostname port)
  (signal-condition! started?)

  (define sessions (make-atomic-box '()))

  (dynamic-wind
    (lambda () 'hi)
    (lambda ()
      (run-fibers
       (lambda ()
         (parameterize ((log log-function))
           (socket-loop socket addr sessions)))
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

