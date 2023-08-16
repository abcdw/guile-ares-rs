(define-module (integration-test)
  #:use-module (nrepl server)
  #:use-module (nrepl client)
  #:use-module (bencode)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-64))

(define-syntax define-test
  (syntax-rules ()
    ((_ test-name e ...)
     (begin
       (define-public (test-name) e ...)
       (set-procedure-property! test-name 'srfi-64-test? #t)))))

;; Hacky workaround for setting up and shutting down nrepl server and client
(define-syntax with-nrepl-setup
  (syntax-rules ()
    ((_ client-socket e ...)
     (let* ((nrepl-server (call-with-new-thread
                           (lambda () (run-nrepl-server #:port 1134))))
            ;; (_ (sleep 1))
            (client-socket (make-nrepl-client-socket #:port 1134))
            (res e ...))
       (when (port? client-socket)
         (close-port client-socket))
       (when (and (thread? nrepl-server) (not (thread-exited? nrepl-server)))
         (cancel-thread nrepl-server)
         (join-thread nrepl-server))
       res))))

(define (nrepl-eval-code client code-str)
  (scm->bencode
    `(("op" . "eval")
      ("code" . ,code-str))
    client)
  (assoc-ref (bencode->scm client) "value"))

(define-test simple-eval
  (with-nrepl-setup nrepl-client
   (test-group "simple eval"
     (test-group "arithmetics"
       (test-equal "(+ 1 2 3)" "6"
                   (nrepl-eval-code nrepl-client "(+ 1 2 3)"))))))

;; TODO: Make it execute both calls properly
(define-public (tmp-fn)
  (with-nrepl-setup
   nrepl-client
   (nrepl-eval-code nrepl-client "(+ 1 2 3)"))

  (with-nrepl-setup
   nrepl-client
   (nrepl-eval-code nrepl-client "(+ 1 2 3)")))

;; (simple-eval)
