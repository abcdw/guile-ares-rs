;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2023-2026 Andrew Tropin <andrew@trop.in>

(define-module (ares nrepl bootstrap-test)
  #:use-module (ares ports)
  #:use-module (ares nrepl bootstrap)
  #:use-module (bencode)
  #:use-module (srfi srfi-1)
  #:use-module ((srfi srfi-197) #:select (chain))
  #:use-module (srfi srfi-64)
  #:use-module (test-utils)
  #:use-module (fibers)
  #:use-module (fibers operations)
  #:use-module (fibers io-wakeup)
  #:use-module (ice-9 match)
  #:use-module (ares-extension ares core)
  #:use-module (ares-extension ares bencode)
  #:use-module (ares-extension nrepl evaluation)
  #:use-module (ares-extension nrepl session))

(define (repl-with-io-port start-repl function)
  (call-with-pipes
   (unbuffer-pipes! (make-pipes 2))
   (match-lambda
     (((input-input-port . input-output-port)
       (output-input-port . output-output-port))
      (run-fibers
       (lambda ()
         (spawn-fiber
          (lambda () (start-repl input-input-port output-output-port)))
         (function input-output-port output-input-port)))))))

(define (read-when-ready port)
  (perform-operation
   (wait-until-port-readable-operation port))
  (bencode->scm port))

(define (exercise-bencode-encoding-error-recovery input-port output-port)
  (define (reply-to-next-message reply)
    ((ares.bencode
      (lambda (context)
        ((assoc-ref context 'reply!) reply)))
     `((ares/input-port . ,input-port)
       (ares/output-port . ,output-port))))

  (reply-to-next-message '(("value" . #f)))
  (reply-to-next-message '(("value" . "ok")
                           ("status" . #("done")))))

(define (encoding-failure-responses)
  (chain
   (string-append
    (scm->bencode-string '(("id" . "1")))
    (scm->bencode-string '(("id" . "2"))))
   (call-with-input-string _
     (lambda (input)
       (call-with-output-string
        (lambda (output)
          (exercise-bencode-encoding-error-recovery input output)))))
   (call-with-input-string _
     (lambda (input)
       (list (bencode->scm input)
             (bencode->scm input))))))

(define (session-repl input-port output-port)
  (bootstrap-nrepl input-port output-port
                   #:initial-extensions
                   (list
                    ares.core
                    ares.bencode
                    nrepl.session)))

(define (compare-messages list1 list2)
 (lset= equal? list1 list2))

(define-test bencode-encoding-failure-test
  (test-equal "Received an error and continued handling replies"
    '((("id" . "1")
       ("status" . #("error" "bencode-encoding-error" "done")))
      (("id" . "2")
       ("value" . "ok")
       ("status" . #("done"))))
    (encoding-failure-responses)))

(define-test session-extension-test
  (test-group "session-extension"
    (repl-with-io-port
     session-repl
     (lambda (input output)
       (scm->bencode `(("op" . "clone")) input)

       (define session-id
         (assoc-ref
          (read-when-ready output) "new-session"))
       (test-assert "Received session-id" session-id)

       (scm->bencode `(("id". "2")
                       ("op" . "eval")
                       ("code" . "(+ 1 2)"))
                     input)
       (test-equal "Received unknow-op"
         `(("id" . "2")
           ("session" . "none")
           ("nrepl/message" ("id" . "2") ("op" . "eval") ("code" . "(+ 1 2)"))
           ("status" . #("error" "unknown-op" "done")))
         (read-when-ready output))

       (scm->bencode `(("id". "3")
                       ("op" . "close")
                       ("session" . ,session-id))
                     input)
       (test-equal "Received session-closed"
         `(("id" . "3")
           ("session" . ,session-id)
           ("status" . #("done" "session-closed")))
         (read-when-ready output))))))

(define (base-repl input-port output-port)
  (bootstrap-nrepl input-port output-port))

(define (missing-symbol-lookup-response)
  (repl-with-io-port
   base-repl
   (lambda (input output)
     (scm->bencode
      '(("id" . "missing-symbol")
        ("ns" . "(ares nrepl bootstrap-test)")
        ("op" . "lookup")
        ("sym" . "ares-definitely-missing-symbol"))
      input)
     (read-when-ready output))))

(define-test missing-symbol-lookup-test
  (test-equal "Missing symbol has empty info"
    '(("id" . "missing-symbol")
      ("session" . "none")
      ("status" . #("done"))
      ("info" . #()))
    (missing-symbol-lookup-response)))

(define-test evaluation-extension-test
  (test-group "evaluation-extension"
    (repl-with-io-port
     base-repl
     (lambda (input output)
       (scm->bencode `(("op" . "clone")) input)

       (define session-id
         (assoc-ref
          (read-when-ready output) "new-session"))
       (test-assert "Received session-id" session-id)

       (scm->bencode `(("id". "2")
                       ("op" . "eval")
                       ("code" . "(+ 1 2)"))
                     input)
       (test-equal "Received error"
         `(("id" . "2")
           ("session" . "none")
           ("status" . #("error" "no-session-id-provided" "done")))
         (read-when-ready output))

       (scm->bencode `(("id". "3")
                       ("op" . "eval")
                       ("code" . "(+ 1 2 3)")
                       ("session" . ,session-id))
                     input)
       (test-equal "Received evaluation value"
         `(("id" . "3")
           ("session" . ,session-id)
           ("value" . "6")
           ("status" . #("done")))
         (read-when-ready output))))))
