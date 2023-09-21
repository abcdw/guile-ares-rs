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

(tree-il->scheme
 (macroexpand
  '(define-module (foo))))
