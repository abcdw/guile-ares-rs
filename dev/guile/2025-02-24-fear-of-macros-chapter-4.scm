(define-module (2025-02-24-fear-of-macros-chapter-4))

(define-syntax our-if
  (lambda (x)
    (syntax-case x ()
      ((_ c e1 e2)
       (syntax (cond (c e1)
                     (else e2)))))))

;; ==

(define-syntax-rule (our-if c e1 e2)
  "There is a doc string for the macro."
  (cond (c e1)
        (else e2)))

(let ((a "hello"))
  (our-if #f (begin (display "hi") a) 2))

;; (define-syntax (hyphen-define/wrong1 stx)
;;   (syntax-case stx ()
;;     [(_ a b (args ...) body0 body ...)
;;      (let ([name (string->symbol (format "~a-~a" #'a #'b))])
;;        #'(define (name args ...)
;;            body0 body ...))]))

(define-syntax hyphen-define
  (lambda (stx)
    ;; (syntax-case stx ()
    ;;   [(_ a b (args ...) body0 body ...)
    ;;    (let ([name (datum->syntax #'a
    ;;                               (symbol-append
    ;;                                (syntax->datum #'a)
    ;;                                '-
    ;;                                (syntax->datum #'b)
    ;;                                ))])
    ;;      #`(define (#,name args ...)
    ;;          body0 body ...))])

    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       #'a
       (with-syntax ([(name _) (datum->syntax
                                #'a
                                (list
                                 'something
                                 (string->symbol
                                  (format #f "~a-~a"
                                          (syntax->datum #'a)
                                          (syntax->datum #'b)))))])
         #'(define (name args ...)
             body0 body ...))])))

(define (syntax->list stx)
  (let loop ((result '())
             (rstx stx))
    (syntax-case rstx ()
      ((elem . rest)
       (loop (cons #'elem result) #'rest))
      (_ (reverse result)))))

#'(1 (+ 4 5) 3)
(syntax->list #'(1 (+ 4 5) 3))

(use-modules (system syntax))
(syntax? (syntax (+ 3 4)))
(syntax? (syntax hi))

(syntax-case stx ()
  ((elem . rest)
   (list #'elem))
  (_ '()))

(define stx #'(if x (list "true") #f))
(syntax->list stx)

(symbol)
(use-modules (srfi srfi-1))

(define-syntax hyphen-define*
  (lambda (stx)
    (syntax-case stx ()
      [(_ (fname names ...) (args ...) body0 body ...)
       (with-syntax ([name (datum->syntax #'fname
                                  ;; (car (syntax->list #'(names ...)))
                                  (fold
                                   (lambda (nm acc)
                                     (symbol-append acc '- (syntax->datum nm)))
                                   (syntax->datum #'fname)
                                   #'(names ...))
                                  )])
         ;; (format #t "~a" name)
         #`(define (name args ...)
             body0 body ...))])))

;; 'hello
;; (define #{some | space symbol}# 123)
;; #{some | space symbol}#

;; #{syntax-object1-syntax-object2}#

(hyphen-define*
 (some1 hyphen-name asterisk) (a b c)
 (list 1 2 3)
 (list 3 2 1)
 (list b c a))

(hyphen-define
 some1 hyphen-name (a b c)
 (list 1 2 3)
 (list 3 2 1)
 (list b c a))

;; (define (some-hyphen-name a b c)
;;  (list 1 2 3)
;;  (list 3 2 1)
;;  (list b c a))

;; (#{#<syntax some1>-#<syntax hyphen-name>}# 5 6 7)

;; (some1-hyphen-name 5 6 7)

(define (format-id stx format-string . rest)
  (datum->syntax
   stx
   (string->symbol
    (apply format #f format-string (map syntax->datum rest)))))

;; (format-id #'"tmp" "~a" #'"other")

(use-modules (ice-9 exceptions))
;; (rnrs exceptions)

(make-syntax-error)
(define-syntax our-struct
  (lambda (stx)
    (syntax-case stx ()
      [(_ id (fields ...))
       (for-each (lambda (x)
                   (unless (identifier? x)
                     (raise-exception
                      (make-exception
                       (make-exception-with-message "Not an identifier: ")
                       (make-syntax-error stx x)))))
                 (cons #'id #'(fields ...)))
       (with-syntax ([pred-id (format-id #'id "~a?" #'id)])
         #`(begin
             ;; Define a constructor.
             (define (id fields ...)
               (apply vector (cons 'id (list fields ...))))
             ;; Define a predicate.
             (define (pred-id v)
               (and (vector? v)
                    (eq? (vector-ref v 0) 'id)))
             ;; Define an accessor for each field.
             #,@(map
                 (lambda (x n)
                   (with-syntax ([acc-id (format-id #'id "~a-~a" #'id x)]
                                 [ix n])
                     #`(define (acc-id v)
                         (unless (pred-id v)
                           (error 'acc-id "~a is not a ~a struct" v 'id))
                         (vector-ref v ix))))
                 #'(fields ...)
                 (iota (length #'(fields ...)) 1))))])))

(our-struct name-of-the-struct ("field1" field2))

(define nots (name-of-the-struct 'hello 'there))

(name-of-the-struct-field2 nots)

(define js `((a . ((b . ((c . "value")))))))
(assoc-ref js 'a)

(define* (alist-refs alist keys #:optional (default #f))
  (let loop ((alist alist)
             (keys keys))
    (if (null? keys)
        alist
        (loop (or (assoc-ref alist (car keys)) default) (cdr keys)))))

(map (lambda (x i) (list x i))
     '(a b c)
     (iota 3 1))

(define-syntax alist.refs
  (lambda (stx)
    (syntax-case stx () ; If the optional ‘default' is missing, use #f.
      [(_)
       (raise-exception
        (make-exception
         (make-exception-with-message
          "Expected alist.key0[.key1 ...] [default]")
         (make-syntax-error stx stx)))]
      [(_ chain)
       #'(alist.refs chain #f)]
      [(_ chain default)
       (unless (identifier? #'chain)
         (raise-exception
          (make-exception
           (make-exception-with-message
            "Expected alist.key0[.key1 ...] [default]")
           (make-syntax-error stx #'chain))))
       (let* ([chain-str (symbol->string (syntax->datum #'chain))]
              [ids (map (lambda (str) (format-id #'chain "~a" str))
                        (string-split chain-str #\.))])
         ;; Check that we have at least hash.key
         ;; (display (cadr ids))
         (unless (and (>= (length ids) 2)
                      (not (equal? (cadr ids) (datum->syntax #'chain (symbol)))))
           (raise-exception
            (make-exception
             (make-exception-with-message
              "Expected alist.key")
             (make-syntax-error stx #'chain))))
         (with-syntax ([collection (car ids)]
                       [keys       (cdr ids)])
           #'(alist-refs collection 'keys default)))])))

(alist.refs js)
(alist.refs js.)
(alist.refs "string")
(alist.refs js.a.test "hello")
(alist.refs 0.a)
