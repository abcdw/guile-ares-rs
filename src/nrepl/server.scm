(define-module (nrepl server)
  #:use-module (bencode))

(use-modules (fibers)
             (scheme base)
             (ice-9 match)
             (ice-9 receive)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-197))



(define (log . args)
  (define (repeat str n)
    (string-join (map (lambda (x) str) (iota n)) " "))
  (if (not (string? (car args)))
      (apply format #t (repeat "~s" (length args)) args)
      (apply format #t args))
  (newline))


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
    (log reply)
    reply)

  ;; TODO: Rewrite to with-exception-handler
  (catch #t
    (lambda () (primitive-eval expression))
    ;; TODO: Return a real exception
    (const "exception happened")
    handle-exception))


;;;
;;; Operations
;;;

(define (response-for input msg)
  (let ((id (assoc-ref input "id"))
        (session (assoc-ref input "session")))
    (chain msg
           (acons "id" (or id "unknown") _)
           (acons "session" (or session "none") _))))

(define (eval-op input)
  (let* ((code (assoc-ref input "code"))
         (result (receive (. vals)
                     (eval-expression (with-input-from-string code read))
                   ;; TODO: handle multiple return values
                   (car vals)))
         (value (format #f "~s" result))
         (response `(("status" . #("done"))
                     ("ns" . "user")
                     ("value" . ,value))))
    (response-for input response)))

(define (clone-op input)
  (response-for
   input
   `(("status" . #("done"))
     ("new-session" . "1"))))

(define (completions-op input)
  (let* ((id (assoc-ref input "id"))
         (response `(("id" . ,id)
                     ("completions" . #()))))
    response))

(define (describe-op input)
  `(lol))

(define default-operations
  `(("eval" . ,eval-op)
    ("describe" . ,describe-op)
    ("completions" . ,completions-op)
    ("clone" . ,clone-op)))

(define (get-operation operations op)
  (assoc-ref operations op))

(define (run-operation operations input)
  (log "input: ~s" input)
  (let* ((op (assoc-ref input "op"))
         (operation (get-operation operations op)))
    (if operation
        (operation input)
        "no-such-operation")))


;;;
;;; loop
;;;

(define (process-request client input)
  ;; Make process request asyncronous
  (;; spawn-fiber
   (lambda ()
     (let ((result (if input (run-operation default-operations input) #f)))
       (log "response: ~s" result)

       (scm->bencode result client))
     (force-output client))))

(define* (client-loop client addr store)
  (setvbuf client 'block 1024)
  ;; Disable Nagle's algorithm.  We buffer ourselves.
  (setsockopt client IPPROTO_TCP TCP_NODELAY 1)

  (log "new connection: ~a" client)

  (let loop ()
    (if (eof-object? (peek-u8 client))
        (begin
          (log "closing connection: ~a" client)
          ;; Don't close until all session are finished
          (close-port client))
        (let ((input (guard (ex (else #f)) (bencode->scm client))))
          (if input (log "input is read") (log "input is malformed"))
          (process-request client input)
          (loop)))))

(define (socket-loop socket addr store)
  (let loop ()
    (match (accept socket SOCK_NONBLOCK)
      ((client . addr)
       (spawn-fiber
        (lambda () (client-loop client addr store)))
       (loop)))))

(define (make-default-socket family addr port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (fcntl sock F_SETFD FD_CLOEXEC)
    (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL)))
    (bind sock family addr port)
    sock))

(define* (run-tcp-nrepl-server socket addr)
  (listen socket 1024)
  (sigaction SIGPIPE SIG_IGN)
  (socket-loop socket addr (make-hash-table)))

(define* (run-nrepl-server
          #:key
          (host #f)
          (family AF_INET)
          (addr (if host (inet-pton family host) INADDR_LOOPBACK))
          (port 7888))
  (define socket (make-default-socket family addr port))
  (define host (gethostbyaddr addr))
  (define hostname (hostent:name host))
  (format #t "nREPL server started on port ~a on host ~a - nrepl://~a:~a\n"
          port hostname hostname port)

  (run-fibers
   (lambda ()
     (run-tcp-nrepl-server socket addr)))
  'my-super-value)
(export run-nrepl-server)

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
