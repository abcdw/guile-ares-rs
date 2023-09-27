(define-module (nrepl drafts)
  #:use-module (bencode)
  #:use-module (fibers)
  #:use-module (nrepl client))

;;;
;;; Module containing experimental code not intended for public use
;;;

(define nrepl-client (make-nrepl-client-socket #:port 38019))
(scm->bencode
 `(("op" . "clone"))
 nrepl-client)

(define session
  (bencode->scm nrepl-client))

(define session-id (assoc-ref session "new-session"))

(begin
  (scm->bencode
   `(("op" . "eval")
     ("id" . "1")
     ("session" . ,session-id)
     ("code" . "(do (println \"hi1\") (Thread/sleep 4000) (println \"finished1\"))"))
   nrepl-client)
  ;; (bencode->scm cider-nrepl) ;; read (out)
  (sleep 1)
  (scm->bencode
   `(("op" . "interrupt")
     ("interrupt-id" . "1")
     ("session" . ,session-id))
   nrepl-client))

(scm->bencode
 `(("op" . "ls-sessions"))
 nrepl-client)
;; (begin
;;   (scm->bencode
;;    `(("op" . "eval")
;;      ("session" . ,session-id)
;;      ("code" . "(do (println \"hi1\") (Thread/sleep 2000) (println \"finished1\"))"))
;;    cider-nrepl)
;;   (scm->bencode
;;    `(("op" . "eval")
;;      ("session" . ,session-id)
;;      ("code" . "(do (println \"hi2\") (Thread/sleep 2000) (println \"finished2\"))"))
;;    cider-nrepl))

(when (char-ready? nrepl-client)
  (bencode->scm nrepl-client))

(define number-cruncher-code
  `(begin
     (define (crunch-numbers n)
       (let loop ((i 0))
         (let l2 ((j 1))
           (if (< j 10000)
               (begin
                 (/ 134 j)
                 (l2 (+ 1 j)))))
         (if (< i n)
             (loop (+ i 1))
             i)))
     (format #t "starting number chrunching!\n")
     (let ((res (crunch-numbers 2000)))
       (format #t "finishing number chrunching!\n")
       res))) ;; 4000 for 10s inside fiber

;;; Pretty-printer

;; https://www.scheme.com/tspl4/records.html
;; (use-modules (rnrs records syntactic))
;; (define-record-type point (fields x y))

;; (record-type-properties point)
;; ((record-accessor (record-type-descriptor point) 'x) (make-point 1 2))

;;; Middlewares
(define (test-mw)
  "ho"
  #((requires . ("session"))
    (expects . ("eval"))
    (handles . (("stdin" . ((doc . "Add content from \"stdin\" to \
current-input-port"))))))
  (display "hi"))
(procedure-properties test-mw)

(use-modules (language tree-il))

(tree-il->scheme
 (macroexpand
  '(define (f a)
     (define b 5)
     (display b))))

(datum->syntax kek 1)
(gensym "prefix-")


;; (tree-il->scheme
;;  (macroexpand
;;   '(define-module (foo))))
;; XXX: Geiser changes evaluation module after this comment

(define (perform effect-name . args)
  (apply abort-to-prompt effect-name args))

(define (handle effect-name handler thunk)
  (call-with-prompt
   effect-name
   thunk
   handler))

(handle
 'flip
 (lambda (k)
   (list (k 1) (k 2)))
 (lambda ()
   ;; (perform 'flip)
   (let ((x (perform 'flip))
         (y (list 3 4)))
     (cons x y))))

;; try swap state and flip handlers
;; https://youtu.be/6lv_E-CjGzg?t=2237
;; (handle
;;  'state
;;  (state-handler)
;;  (lambda ()
;;    (handle
;;     'flip
;;     (handle-flip)

;;     (lambda ()
;;       (if (perform 'flip)
;;           (begin
;;             (perform 'update 1+)
;;             (perform 'get))
;;           (begin
;;             (perform 'update 1+)
;;             (perform 'get)))))))



;; (define* (run-repl
;;           input-channel
;;           output-channel
;;           #:key
;;           (extensions
;;            (list
;;             state-extension
;;             bencode-extension
;;             session-extension)))

;;   )

;; (define (make-channel-port input-channel output-channel)
;;   (define position 0)

;;   (define (read! bv start count)

;;     (let ((count (min count (- length position))))
;;       (bytevector-copy! source position
;;                         bv start count)
;;       (set! position (+ position count))
;;       count))
;;   (make-custom-binary-input/output-port
;;    id read! write! get-position set-position! close))


(use-modules ;; (scheme base)
             (rnrs bytevectors)
             (ice-9 binary-ports))

(use-modules (ice-9 textual-ports)
             (fibers conditions)
             (fibers channels))

(define* (open-bytevector-input-port
          #:key
          (condition (make-condition))
          (channel (make-channel)))
  (define position 0)

  (define (read! bv start count)
    (signal-condition! condition)
    ;; (put-message channel 'required)
    (let* ((str ;; (get-message source-channel)
                "tmp")
           (bytes (string->utf8 str))
           (count (min count (- (bytevector-length bytes) position))))
      (bytevector-copy! bytes position
                        bv start count)
      (set! position (+ position count))
      count))

  (define (get-position) position)

  (define (set-position! new-position)
    (set! position new-position))

  (make-custom-binary-input-port "the port" read!
                                  get-position set-position!
                                  #f))


;;;
;;; REPL
;;;

(define (self-quoting? x)
  "Return #t if X is self-quoting."
  (letrec-syntax ((one-of (syntax-rules ()
                            ((_) #f)
                            ((_ pred rest ...)
                             (or (pred x)
                                 (one-of rest ...))))))
    (one-of symbol? string? keyword? pair? null? array?
            number? boolean? char?)))

(define (value->sexp value)
    (if (self-quoting? value)
        `(value ,value)
        `(non-self-quoting ,(object-address value)
                           ,(object->string value))))

(define (stack->frames stack)
  "Return STACK's frames as a list."
  (unfold (cute >= <> (stack-length stack))
          (cut stack-ref stack <>)
          1+
          0))

(define (frame->sexp frame)
  `(,(frame-procedure-name frame)
    ,(match (frame-source frame)
       ((_ (? string? file) (? integer? line) . (? integer? column))
        (list file line column))
       (_
        '(#f #f #f)))))

(define (nrepl-prompt-message)
  "scheme@(module-here-someday)> ")

(define (eval-expression expression)
  (define (handle-exception key . args)
    (define reply
      (let ((stack (make-stack #t)
                   ;; (if (nrepl-prompt)
                   ;;     (make-stack #t handle-exception (nrepl-prompt))
                   ;;     (make-stack #t))
                   ))
        ;; Note: 'make-stack' returns #f if there's no 'handle-exception'
        ;; stack frame, which is the case when this file is being
        ;; interpreted as with 'primitive-load'.
        `(exception (arguments ,key ,@(map value->sexp args))
                    (stack ,@(map frame->sexp
                                  (if stack
                                      (stack->frames stack)
                                      '()))))))
    ((log) "repl result: ~s" reply)
    reply)

  ;; TODO: Rewrite to with-exception-handler
  (catch #t
    (lambda () (primitive-eval expression))
    ;; TODO: Return a real exception
    (const "exception happened")
    handle-exception))

(define (read-expression code)
  (catch #t
    (lambda () (with-input-from-string code read))
    ;; TODO: Return a real exception and handle it properly
    (const "Couldn't read the code")))


