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
    (test-equal "-42" "i-42e" (scm->bencode-string -42)))
  (test-group "strings"
    (test-equal "\"spam\"" "4:spam" (scm->bencode-string "spam"))
    (test-equal "empty string" "0:" (scm->bencode-string "")))
  (test-group "lists"
    (test-equal "empty list" "le" (scm->bencode-string '()))
    (test-equal "list with number and string"
      "l4:spami-42ee" (scm->bencode-string '("spam" -42)))
    (test-equal "list with nested list, number and string"
      "ll4:spamelei-42ee" (scm->bencode-string '(("spam") () -42)))))

(define-test decode
  (test-group "integers"
    (test-equal "i0e :: zero" 0 (bencode-string->scm "i0e"))
    (test-error "i0a :: unexpected character" #t (bencode-string->scm "i0a"))
    (test-equal "i42e :: positive number" 42 (bencode-string->scm "i42e"))
    (test-equal "i-42e :: negative number" -42 (bencode-string->scm "i-42e"))
    (test-error "i4-2e :: unexpected character" #t (bencode-string->scm "i4-2e"))
    (test-error "i--4e :: unexpected character" #t (bencode-string->scm "i--4e"))
    (test-error "ie :: no digits" #t (bencode-string->scm "ie"))
    (test-error "i-e :: no digits" #t (bencode-string->scm "i-e"))))
