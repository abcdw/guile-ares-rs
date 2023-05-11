(define-module (bencode)
  #:export (scm->bencode
            scm->bencode-string
            bencode->scm
            bencode-string->scm))

(define (bencode-exception port)
  (throw 'bencode-invalid port))

(define (integer->bencode int)
  (format #f "i~ae" int))

(define* (scm->bencode scm #:optional (port (current-output-port)))
  (display (integer->bencode scm) port))

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

(define (read-bencode-integer port)
  (expect-char (lambda (x) (char=? #\i x)) port)
  (read-char port)
  (let loop ((ch (read-char port))
             (res ""))
    (cond ((and (char=? #\e ch)
                (not (string-null? res))
                (not (string=? res "-")))
           (string->number res))

          ((or
            (digit? ch)
            (and (char=? #\- ch) (string-null? res)))
           (loop (read-char port) (string-append res (string ch))))

          (else
           (bencode-exception port)))))

(define* (bencode->scm #:optional (port (current-input-port)))
  ;; (peek-char )
  (read-bencode-integer port)
  )

(define* (bencode-string->scm str)
  (call-with-input-string str
    (lambda (p)
      (bencode->scm p))))

;; (bencode-string->scm "i43e")
