(define-module (bencode-test)
  #:use-module (bencode)
  #:use-module (srfi srfi-64))

(define-syntax define-test
  (syntax-rules ()
    ((_ test-name e ...)
     (begin
       (define-public (test-name) e ...)
       (set-procedure-property! test-name 'srfi-64-test? #t)))))

(define-test encode
  (test-group "integers"
    (test-equal "0" "i0e" (scm->bencode-string 0))
    (test-equal "42" "i42e" (scm->bencode-string 42))
    (test-equal "-42" "i-42e" (scm->bencode-string -42))))
