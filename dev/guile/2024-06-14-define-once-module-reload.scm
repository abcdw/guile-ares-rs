(define-module (2024-06-14-define-once-module-reload)
  #:use-module (ice-9 atomic)
  #:use-module (ares atomic))

(define-syntax-rule (define-once sym val)
  (define sym
    (if (module-locally-bound? (current-module) 'sym) sym val)))

;; (module-locally-bound? (current-module) 'state)

(define-once my-state (make-atomic-box '()))

(define my-state my-state)
;; (set! state (make-atomic-box '()))
(define not-state (make-atomic-box '()))

;; (atomic-box-update! state (lambda (x) (cons (length x) x)))
;; (atomic-box-update! not-state (lambda (x) (cons (length x) x)))

;; (format #t "my-state: ~a\nnot-state: ~a\n" my-state not-state)
(format #t "~y"
        (hash-map->list
         (lambda (k v) (cons k v))
         (module-obarray (resolve-module '(2024-06-14-define-once-module-reload)))))

(define (reload)
  (let ((m (resolve-module '(2024-06-14-define-once-module-reload))))
    (module-clear! m)
    ;; (reload-module m)
    ))
;; https://youtu.be/PEDTXF6SQeE
