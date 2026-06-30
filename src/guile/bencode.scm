;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2023-2026 Andrew Tropin <andrew@trop.in>

(define-module (bencode)
  ;; TODO: Make explicit overrides if possible to supress warnings
  ;; This is too much broad, better to list somehow all functions that
  ;; we are ovirriding
  #:duplicates (replace last) ; for-each
  #:use-module (scheme base)
  #:use-module (ice-9 exceptions)

  #:export (&bencode-encoding-exception
            make-bencode-encoding-exception
            bencode-encoding-exception?
            bencode-encoding-exception-value
            &bencode-decoding-exception
            make-bencode-decoding-exception
            bencode-decoding-exception?
            bencode-decoding-exception-port
            scm->bencode
            scm->bencode-string
            bencode->scm
            bencode-string->scm))

(define-exception-type
  &bencode-decoding-exception &exception
  make-bencode-decoding-exception
  bencode-decoding-exception?
  (port bencode-decoding-exception-port))

(define-exception-type
  &bencode-encoding-exception &exception
  make-bencode-encoding-exception
  bencode-encoding-exception?
  (value bencode-encoding-exception-value))

(define (raise-bencode-encoding-exception value)
  (raise-exception
   (make-exception
    (make-bencode-encoding-exception value)
    (make-exception-with-message "value cannot be bencoded")
    (make-exception-with-irritants value))))

(define (raise-bencode-decoding-exception port)
  (raise-exception
   (make-exception
    (make-bencode-decoding-exception port)
    (make-exception-with-message "bencode is invalid")
    (make-exception-with-irritants port))))

(define (write-u8-char char port)
  (write-u8 (char->integer char) port))

(define (write-u8-integer int port)
  (write-bytevector (string->utf8 (number->string int)) port))

(define (write-bencode-integer int port)
  (when (not (integer? int))
    (raise-bencode-encoding-exception int))
  (write-u8-char #\i port)
  (write-u8-integer int port)
  (write-u8-char #\e port))

(define (string-like? scm)
  (or (string? scm)
      (keyword? scm)
      (symbol? scm)))

(define (write-bencode-string str port)
  (when (not (string-like? str))
    (raise-bencode-encoding-exception str))
  (let* ((str (cond
               ((keyword? str) (symbol->string (keyword->symbol str)))
               ((symbol? str) (symbol->string str))
               (else str)))
         (utf8-str (string->utf8 str))
         (utf8-str-len (bytevector-length utf8-str)))
    (write-u8-integer utf8-str-len port)
    (write-u8-char #\: port)
    (write-bytevector utf8-str port)))

(define (write-bencode-list vec port)
  (write-u8-char #\l port)
  (vector-for-each (lambda (x) (scm->bencode x port)) vec)
  (write-u8-char #\e port))

(define (write-bencode-dictionary lst port)
  (write-u8-char #\d port)
  (for-each
   (lambda (x)
     (when (not (pair? x))
       (raise-bencode-encoding-exception x))
     (write-bencode-string (car x) port)
     (scm->bencode (cdr x) port))
   lst)
  (write-u8-char #\e port))

(define* (scm->bencode scm #:optional (port (current-output-port)))
  (cond
   ((integer? scm)
    (write-bencode-integer scm port))
   ((string-like? scm)
    (write-bencode-string scm port))
   ((vector? scm)
    (write-bencode-list scm port))
   ((list? scm)
    (write-bencode-dictionary scm port))
   (else
    (raise-bencode-encoding-exception scm))))

(define* (scm->bencode-string scm)
  (call-with-output-string
   (lambda (p)
     (scm->bencode scm p))))

(define (expect-char pred port)
  (let ((ch (peek-char port)))
    (cond
     ((not (pred ch)) (raise-bencode-decoding-exception port))
     ((eof-object? ch) (raise-bencode-decoding-exception port)))))

(define (digit? c)
  (case c
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) #t)
    (else #f)))

(define* (read-bencode-integer
          port
          #:key (delimeter #\e))
  (let loop ((ch (read-char port))
             (res ""))
    (cond ((eof-object? ch)
           (raise-bencode-decoding-exception port))

          ((and (char=? delimeter ch)
                (not (string-null? res))
                (not (string=? res "-")))
           (string->number res))

          ((or
            (digit? ch)
            (and (char=? #\- ch) (string-null? res)))
           (loop (read-char port) (string-append res (string ch))))

          (else
           (raise-bencode-decoding-exception port)))))

(define (read-bencode-string port)
  (let* ((str-length (read-bencode-integer port #:delimeter #\:))
         (res (read-bytevector str-length port)))
    (if (and (bytevector? res)
             (= (bytevector-length res) str-length))
        (utf8->string res)
        (raise-bencode-decoding-exception port))))

(define (read-bencode-list port)
  (let loop ((res #())
             (next-ch (peek-char port)))
    (cond
     ((eof-object? next-ch)
      (raise-bencode-decoding-exception port))

     ((char=? #\e next-ch)
      (read-char port)
      res)

     (else
      (loop
       (vector-append res `#(,(bencode->scm port)))
       (peek-char port))))))

(define (read-bencode-dictionary port)
  (let loop ((res '())
             (next-ch (peek-char port)))
    (cond
     ((eof-object? next-ch)
      (raise-bencode-decoding-exception port))

     ((char=? #\e next-ch)
      (read-char port)
      (reverse res))

     (else
      (loop
       (cons (cons (read-bencode-string port) (bencode->scm port)) res)
       (peek-char port))))))

(define* (bencode->scm #:optional (port (current-input-port)))
  (let ((ch (peek-char port)))
    (cond
     ((eof-object? ch)
      (raise-bencode-decoding-exception port))

     ((char=? #\i ch)
      (read-char port)
      (read-bencode-integer port))
     ((char=? #\l ch)
      (read-char port)
      (read-bencode-list port))
     ((char=? #\d ch)
      (read-char port)
      (read-bencode-dictionary port))
     (else
      (read-bencode-string port)))))

(define* (bencode-string->scm str)
  (call-with-input-string str
    (lambda (p)
      (bencode->scm p))))

;; end of bencode.scm
