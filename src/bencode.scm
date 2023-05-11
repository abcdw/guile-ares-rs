(define-module (bencode)
  #:export (scm->bencode
            scm->bencode-string))

(define* (scm->bencode scm #:optional (port (current-output-port)))
  (display "hi" port))

(define* (scm->bencode-string scm)
  (call-with-output-string
   (lambda (p)
     (scm->bencode scm p))))
