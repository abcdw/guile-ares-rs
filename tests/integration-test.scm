(define-module (integration-test)
  #:use-module (nrepl server)
  #:use-module (nrepl client)
  #:use-module (bencode)
  #:use-module (fibers)
  #:use-module (ice-9 threads)
  #:use-module (fibers conditions)
  #:use-module (srfi srfi-64)
  #:use-module (test-utils))

(define* (call-with-nrepl-setup proc #:key (port 1134))
  (let* ((server-started? (make-condition))
         (nrepl-server (call-with-new-thread
                        (lambda ()
                          (run-nrepl-server
                           #:port port
                           #:started? server-started?))))
         (_ (wait server-started?))
         (client-socket (make-nrepl-client-socket #:port port)))
    (call-with-values
        (lambda () (proc client-socket))
      (lambda vals
        (close-port client-socket)
        (cancel-thread nrepl-server)
        (join-thread nrepl-server)
        (apply values vals)))))

(define (nrepl-eval-code client code-str)
  (scm->bencode
    `(("op" . "eval")
      ("code" . ,code-str))
    client)
  (assoc-ref (bencode->scm client) "value"))

(define-public (create-socket-in-thread-in-fiber)
  (run-fibers
   (lambda ()
     (define th
       (call-with-new-thread
        (@ (nonblocking-socket) create-socket-exception)))
     (join-thread th))))

(define-test nonblocking-socket-exception
  ;; https://github.com/wingo/fibers/issues/105
  ;; Should work the same as `make fine-socket`
  (test-expect-fail 1)
  (test-equal "nonblocking-socket created, but connection refused"
    'system-error
    (create-socket-in-thread-in-fiber)))

;; This is a draft/stub for future implementation of integration tests
(define-test simple-eval
  (call-with-nrepl-setup
   (lambda (nrepl-client)
     (define (send-message message)
       (scm->bencode message nrepl-client))
     (define (receive-message)
       (bencode->scm nrepl-client))

     (send-message `(("op" . "clone")))
     (define session-id (assoc-ref (receive-message) "new-session"))
     (test-group "session"
       (test-group "clone"
         (test-assert "New session returned" session-id)))

     (send-message
      `(("code" . "(+ 1 2 3)")
        ("session" . ,session-id)
        ("op" . "eval")))

     (test-group "simple eval"
       (test-group "arithmetics"
         (test-equal "code for evaluation: (+ 1 2 3)"
           "6"
           (assoc-ref (receive-message) "value"))))

     (send-message
      `(("code" . "((@ (nonblocking-socket) create-socket-exception))")
        ("session" . ,session-id)
        ("op" . "eval")))

     (test-group "nonblocking socket"
       ;; https://todo.sr.ht/~abcdw/tickets/7
       ;; Should return connection refused (system-error)
       (test-equal "create socket"
         "system-error"
         (assoc-ref (receive-message) "value")))

     ;; (test-group "read error"
     ;;   (test-group "arithmetics"
     ;;     (test-equal "code for evaluation: (+ 1 2 3"
     ;;       "\"Couldn't read the code\""
     ;;       (nrepl-eval-code nrepl-client "(+ 1 2 3"))))

     ;;        (test-group "simple interrupt workflow"
     ;;          (scm->bencode
     ;;           `(("op" . "clone"))
     ;;           nrepl-client)
     ;;          (define session-id
     ;;            (assoc-ref (bencode->scm nrepl-client) "new-session"))
     ;;          (test-assert "clone session" session-id)
     ;;          (scm->bencode
     ;;           `(("op" . "eval")
     ;;             ("id" . "1")
     ;;             ("session" . ,session-id)
     ;;             ("code" . "(begin
     ;; (+ 2 3)
     ;; (sleep 1)
     ;; 'evaluation-result)"))
     ;;           nrepl-client)
     ;;          (scm->bencode
     ;;           `(("op" . "interrupt")
     ;;             ("interrupt-id" . "1")
     ;;             ("session" . ,session-id))
     ;;           nrepl-client)
     ;;          (test-equal "obtained interrupted status"
     ;;            #("done" "interrupted")
     ;;            (assoc-ref
     ;;             (bencode->scm nrepl-client)
     ;;             "status")))
     )))
