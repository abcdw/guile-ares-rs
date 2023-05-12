(define-module (bencode)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 textual-ports)
  #:export (scm->bencode
            scm->bencode-string
            bencode->scm
            bencode-string->scm))

(define (bencode-exception port)
  (throw 'bencode-invalid port))

(define (integer->bencode int port)
  (when (not (integer? int))
    (bencode-exception port))
  (format port "i~ae" int))

(define (string->bencode str port)
  (when (not (string? str))
    (bencode-exception port))
  (format port "~a:~a" (string-length str) str))

(define (vector->bencode lst port)
  (format port "l")
  (vector-for-each (lambda (i x) (scm->bencode x port)) lst)
  (format port "e"))

(define (alist->bencode lst port)
  (format port "d")
  (for-each
   (lambda (x)
     (when (not (pair? x))
       (bencode-exception port))
     (string->bencode (car x) port)
     (scm->bencode (cdr x) port))
   lst)
  (format port "e"))

(define* (scm->bencode scm #:optional (port (current-output-port)))
  (cond
   ((integer? scm)
    (integer->bencode scm port))
   ((string? scm)
    (string->bencode scm port))
   ((vector? scm)
    (vector->bencode scm port))
   ((list? scm)
    (alist->bencode scm port))))

(define* (scm->bencode-string scm)
  (call-with-output-string
   (lambda (p)
     (scm->bencode scm p))))

(define (expect-char pred port)
  (let ((ch (peek-char port)))
    (cond
     ((not (pred ch)) (bencode-exception port))
     ((eof-object? ch) (bencode-exception port)))))

(define (digit? c)
  (case c
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) #t)
    (else #f)))

(define* (read-bencode-integer
          port
          #:key (delimeter #\e))
  (let loop ((ch (read-char port))
             (res ""))
    (cond ((and (char=? delimeter ch)
                (not (string-null? res))
                (not (string=? res "-")))
           (string->number res))

          ((or
            (digit? ch)
            (and (char=? #\- ch) (string-null? res)))
           (loop (read-char port) (string-append res (string ch))))

          (else
           (bencode-exception port)))))

(define (read-bencode-string port)
  (let* ((str-length (read-bencode-integer port #:delimeter #\:))
         (res (get-string-n port str-length)))
    (if (= (string-length res) str-length)
        res
        (bencode-exception port))))

(define (read-bencode-list port)
  (let loop ((res #())
             (next-ch (peek-char port)))
    (cond
     ((char=? #\e next-ch)
      (read-char port)
      res)

     (else
      (loop
       (vector-append res `#(,(bencode->scm port)))
       (peek-char port))))))

(define* (bencode->scm #:optional (port (current-input-port)))
  (let ((ch (peek-char port)))
    (cond
     ((char=? #\i ch)
      (read-char port)
      (read-bencode-integer port))
     ((char=? #\l ch)
      (read-char port)
      (read-bencode-list port))
     (else
      (read-bencode-string port)))))

(define* (bencode-string->scm str)
  (call-with-input-string str
    (lambda (p)
      (bencode->scm p))))

;; (bencode-string->scm "l2:hilei34ee")
