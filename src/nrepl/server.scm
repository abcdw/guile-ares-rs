(define-module (nrepl server)
  #:use-module (bencode))

(use-modules (fibers)
             (rnrs bytevectors)
             (ice-9 match)
             (ice-9 textual-ports)
             (ice-9 rdelim)

             (srfi srfi-26)
             (srfi srfi-1))

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

(define (log str)
  (display str)
  (newline)
  ;; (let* ((file "nrepl.log")
  ;;        (output-port (open-file file "a")))
  ;;   (display str output-port)
  ;;   (newline output-port)
  ;;   (close output-port))
  )

(define nrepl-prompt
  (make-parameter (lambda () "scheme@(module-here-someday)> ")))

(define (eval-expression output expression)
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

    (write reply output)
    (newline output)
    (force-output output))

  (catch #t
    (lambda ()
      (let ((results (call-with-values (lambda () (primitive-eval expression))
                       list)))
        (write `(values ,@(map value->sexp results)) output)
        (newline output)
        (force-output output)))
    (const #t)
    handle-exception))

(define* (client-loop client addr store)
  (setvbuf client 'block 1024)
  ;; Disable Nagle's algorithm.  We buffer ourselves.
  (setsockopt client IPPROTO_TCP TCP_NODELAY 1)

  (log (format #f "new connection: ~a" client))

  (let loop ()
    (put-string client ((nrepl-prompt)))
    (force-output client)
    (let ((line (read-line client)))
      (cond
       ((eof-object? line)
        (close-port client))
       (else
        (log (format #f "new request: ~a" line))
        (let* ((input
                (catch #t
                  (lambda ()
                    (bencode-string->scm line))
                  (const "invalid-bencode")))
               (_ (log (format #f "input: ~a" input)))
               (op (assoc-ref input "op"))
               (exp (if (and op (string=? "eval" op))
                        (with-input-from-string (assoc-ref input "code") read)
                        "'not-eval-op")))
          (eval-expression client exp))
        (force-output client)
        (loop))))))

(define op (current-output-port))

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
          (port 11211))
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
