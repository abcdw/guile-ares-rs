(define-module (nrepl)
  #:use-module (bencode))

(use-modules (fibers)
             (rnrs bytevectors)
             (ice-9 match)
             (ice-9 textual-ports)
             (ice-9 rdelim)

             (srfi srfi-26)
             (srfi srfi-1))

(define nrepl-prompt
  (make-parameter (lambda () "scheme@(module-here-someday)> ")))

(define (make-default-socket family addr port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (fcntl sock F_SETFD FD_CLOEXEC)
    (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL)))
    (bind sock family addr port)
    sock))

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

(define (client-loop port addr store)
  (setvbuf port 'block 1024)
  ;; Disable Nagle's algorithm.  We buffer ourselves.
  (setsockopt port IPPROTO_TCP TCP_NODELAY 1)

  (define (handle-exception key . args)
    (define reply
      (match version
        ((0 1 (? positive?) _ ...)
         ;; Protocol (0 1 1) and later.
         (let ((stack (if (nrepl-prompt)
                          (make-stack #t handle-exception (nrepl-prompt))
                          (make-stack #t))))
           ;; Note: 'make-stack' returns #f if there's no 'handle-exception'
           ;; stack frame, which is the case when this file is being
           ;; interpreted as with 'primitive-load'.
           `(exception (arguments ,key ,@(map value->sexp args))
                       (stack ,@(map frame->sexp
                                     (if stack
                                         (stack->frames stack)
                                         '()))))))
        (_
         ;; Protocol (0 0).
         `(exception ,key ,@(map value->sexp args)))))

    (write reply port)
    (newline port)
    (force-output port))

  (let loop ()
    ;; TODO: Restrict read-line to 512 chars.
    (put-string port ((nrepl-prompt)))
    (force-output port)
    (let ((line (read-line port)))
      (cond
       ((eof-object? line)
        (close-port port))
       (else
        (let ((exp (with-input-from-string line read)))
          (catch #t
            (lambda ()
              (let ((results (call-with-values
                                 (lambda ()
                                   (primitive-eval exp))
                               list)))
                (write `(values ,@(map value->sexp results)) port)
                (newline port)
                (force-output port)))
            (const #t)
            handle-exception))
        (put-char port #\newline)
        (force-output port)
        (loop))))))

(define (socket-loop socket store)
  (let loop ()
    (match (accept socket SOCK_NONBLOCK)
      ((client . addr)
       (spawn-fiber (lambda () (client-loop client addr store)))
       (loop)))))

(define host #f)
(define family AF_INET)
(define addr (if host (inet-pton family host) INADDR_LOOPBACK))
(define port 11211)

(define-once nrepl-socket
  (make-default-socket family addr port))

(define* (run-ping-server
          #:key
          (socket nrepl-socket))
  (listen socket 1024)
  (sigaction SIGPIPE SIG_IGN)
  (socket-loop socket (make-hash-table)))

(use-modules (ice-9 threads))

(define (hi-fn)
  (display "starting fibers server...")
  (run-fibers run-ping-server)
  'my-super-value)

(define server-thread #f)

(define (restart-server-thread)
  (when (thread? server-thread)
    (cancel-thread server-thread))

  (set! server-thread (call-with-new-thread hi-fn)))

(restart-server-thread)

;; server-thread
;; (cancel-thread server-thread)
;; (join-thread server-thread 1)
