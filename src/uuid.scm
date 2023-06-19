(define-module (uuid)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-197)
  #:export (uuid))

(define (gen-rands len)
  (let loop ((len len)
             (acc '()))
    (if (positive? len)
        (loop (1- len) (cons (random 16) acc))
        acc)))

(define (list->hex-str l)
  (chain l
    (map (cut number->string <> 16) _)
    (apply string-append _)))

(define (uuid)
  (chain '(8 4 (#:version 3) (#:reserved 3) 12)
    (map (match-lambda
           ((? number? n) (gen-rands n))
           ((#:version (? number? n)) (cons 4 (gen-rands n)))
           ((#:reserved (? number? n)) (chain (random 16)
                                         (logand (lognot (ash 1 2)) _)
                                         (logior (ash 1 3) _)
                                         (cons _ (gen-rands n))))) _)
    (map list->hex-str _)
    (string-join _ "-")))

;; Local Variables:
;; eval: (put 'chain 'scheme-indent-function 'defun)
;; End:
