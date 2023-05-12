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
  (test-group "encode"
    (test-group "integers"
      (test-equal "0" "i0e" (scm->bencode-string 0))
      (test-equal "42" "i42e" (scm->bencode-string 42))
      (test-equal "-42" "i-42e" (scm->bencode-string -42)))

    (test-group "strings"
      (test-equal "\"spam\"" "4:spam" (scm->bencode-string "spam"))
      (test-equal "empty string" "0:" (scm->bencode-string "")))

    (test-group "lists"
      (test-equal "empty list" "le" (scm->bencode-string #()))
      (test-equal "list with number and string"
        "l4:spami-42ee" (scm->bencode-string #("spam" -42)))
      (test-equal "list with nested list, number and string"
        "ll4:spamelei-42ee" (scm->bencode-string #(#("spam") #() -42))))

    (test-group "dictionaries"
      (test-equal "empty dict" "de" (scm->bencode-string '()))
      (test-equal "dict with number and string"
        "d4:spami-42ee" (scm->bencode-string '(("spam" . -42))))
      (test-error
       "dict with non-string key"
       #t (scm->bencode-string '((42 . -42))))
      (test-equal "dict without value"
        "d4:listde4:spami-42ee" (scm->bencode-string '(("list")
                                                       ("spam" . -42))))
      (test-error
       "malformed dict with empty key"
       #t (scm->bencode-string '(("spam") ())))
      (test-error
       "malformed dict with number"
       #t (scm->bencode-string '(("spam") -42))))))

(define-test decode
  (test-group "decode"
    (test-group "integers"
      (test-equal "i0e :: zero" 0 (bencode-string->scm "i0e"))
      (test-error "i0a :: unexpected character" #t (bencode-string->scm "i0a"))
      (test-equal "i42e :: positive number" 42 (bencode-string->scm "i42e"))
      (test-equal "i-42e :: negative number" -42 (bencode-string->scm "i-42e"))
      (test-error "i4-2e :: unexpected character" #t (bencode-string->scm "i4-2e"))
      (test-error "i--4e :: unexpected character" #t (bencode-string->scm "i--4e"))
      (test-error "ie :: no digits" #t (bencode-string->scm "ie"))
      (test-error "i-e :: no digits" #t (bencode-string->scm "i-e"))
      (test-error "i :: unexpcted EOF" #t (bencode-string->scm "i"))
      (test-error "i-12 :: unexpcted EOF" #t (bencode-string->scm "i-12")))

    (test-group "strings"
      (test-equal "0: :: empty string"
        "" (bencode-string->scm "0:"))
      (test-equal "4:spam :: simple string"
        "spam" (bencode-string->scm "4:spam"))
      (test-equal "2:sp2:am :: two strings"
        "sp" (bencode-string->scm "2:sp2:am"))
      (test-error "4:spa :: unexpected EOF" #t (bencode-string->scm "4:spa")))

    (test-group "lists"
      (test-equal "le :: empty list"
        #() (bencode-string->scm "le"))
      (test-equal "l4:spami34ee :: simple list with string and integer"
        #("spam" 34) (bencode-string->scm "l4:spami34ee"))
      (test-equal "lllel2:hilei34eeee :: nested lists"
        #(#() #("hi" #() 34)) (bencode-string->scm "llel2:hilei34eee"))
      (test-error "l :: unexpected EOF" #t (bencode-string->scm "l")))))
