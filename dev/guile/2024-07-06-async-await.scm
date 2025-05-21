(define-module (2024-07-06-async-await))

(define (.then l func)
  (apply append (map func l)))

(define (pure x)
  (list x))

(define prompt-tag (make-prompt-tag))

;; Awaiting a value (choosing a value among nondeterministic choices)
;; is simple, just abort to the nearest handler and give the list.
(define (await mval)
  (abort-to-prompt prompt-tag
                   mval))

;; When the continuation is to be threaded using nondeterministic
;; value (a list of things), we use .then on the continuation while
;; making sure we re-delimit the end of the continuation using another
;; async block.
(use-modules (srfi srfi-1))
(define (async thunk)
  (call-with-prompt
   prompt-tag
   thunk
   (lambda (cont value)
     (append-map
      (lambda (v)
        (async (lambda () (cont v))))
      value))))

(use-modules (ice-9 pretty-print))

(pretty-print
 (async (lambda ()
          (let ((num (await '(1 2 3)))
                (letter (await '(a b)))
                (fruit (await '("pomme" "orange" "banane"))))
            (pure (list num letter fruit))))))
