(define-module (2024-05-30-dynamically-scoped-variables))

;;;
;;; Dynamically Scoped Variables
;;;

;; library-x

(define greeting-template (make-fluid "Hello ~a!"))
(define greeting
  (lambda (user)
    (format #t (fluid-ref greeting-template) user)
    (newline)))

(define questionable-function
  (lambda (question user)
    (greeting user)
    (display question)
    (newline)))
;; library-x ends here


;;; 1. Lexical Scope

(greeting "Alice")

(questionable-function "How are you?" "Bob")

(questionable-function "Wie geht es dir?" "Bob")

;; What options we have?

(let ((greeting-template "Guten Tag ~a!"))
  (format #t "greeting-template: ~s\n" greeting-template)
  (questionable-function "Wie geht es dir?" "Bob"))



;;; 2. Dynamic Scope

;; a small refactoring needed
(with-fluids ((greeting-template "Bonjour ~a!"))
  (fluid-ref greeting-template))

(with-fluids ((greeting-template "Bonjour ~a!"))
  (questionable-function "Wie geht es dir?" "Bob"))

(questionable-function "How are you?" "Bob")



;;; 3. Dynamic State

(define ds (current-dynamic-state))

(with-dynamic-state ds
  (lambda () (greeting "bob")))

(define ds-redefined
  (with-fluids ((greeting-template "Bonjour ~a!"))
    (current-dynamic-state)))

(with-dynamic-state ds-redefined
  (lambda () (greeting "bob")))

;;; 4. Thread-Local Fluids

(define tlf (make-thread-local-fluid 'value))

(define ds-tl
  (with-fluids ((tlf 'another-value))
               (fluid-ref tlf)
               (current-dynamic-state)))

(with-dynamic-state
 ds-tl
 (lambda () (fluid-ref tlf)))



;;; 5. Parameters

(define tmp-param
  (make-parameter "hello" (lambda (p)
                            (unless (string? p)
                              (error "@code{p} should be string"))
                            (string-append "converted " p))))
(tmp-param 12)
(tmp-param "goodbye")

(parameterize ((tmp-param "hehe"))
  (tmp-param))
(tmp-param)

;; (fluid->parameter FLUID CONVERSION-FUNCTTION)

;; use with caution







;; (hash-map->list (lambda (x y) (cons x y)) (module-obarray (current-module)))


;; (module-clear! (current-module))
