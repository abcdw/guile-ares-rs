(define-module (integration-test)
  #:use-module (nrepl server)
  #:use-module (nrepl client)
  #:use-module (bencode)
  #:use-module (ice-9 threads)
  #:use-module (fibers conditions)
  #:use-module (srfi srfi-64))

(use-modules (gider test-runners))
(test-runner-current (test-runner-create))

(define-syntax define-test
  (syntax-rules ()
    ((_ test-name e ...)
     (begin
       (define-public (test-name) e ...)
       (set-procedure-property! test-name 'srfi-64-test? #t)))))

;; (call-with-nrepl-setup (lambda (client) (display "hi=======>\n")))

;; Hacky workaround for setting up and shutting down nrepl server and client
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

;; This a draft/stub for future implementation of integration tests
(define-test simple-eval
  (call-with-nrepl-setup
   (lambda (nrepl-client)
     (begin
       (test-group "simple eval"
         (test-group "arithmetics"
           (test-equal "code for evaluation: (+ 1 2 3)"
             "6"
             (nrepl-eval-code nrepl-client "(+ 1 2 3)"))))
       (test-group "read error"
         (test-group "arithmetics"
           (test-equal "code for evaluation: (+ 1 2 3"
             "\"Couldn't read the code\""
             (nrepl-eval-code nrepl-client "(+ 1 2 3"))))))))

;; TODO: Make it execute both calls properly
;; (define-public (tmp-fn)
;;   (with-nrepl-setup
;;    nrepl-client
;;    (nrepl-eval-code nrepl-client "(+ 1 2 3)"))

;;   (with-nrepl-setup
;;    nrepl-client
;;    (nrepl-eval-code nrepl-client "(+ 1 2 3)")))


;; (simple-eval)
