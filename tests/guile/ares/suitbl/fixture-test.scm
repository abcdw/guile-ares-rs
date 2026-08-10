;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl fixture-test)
  #:use-module (ares suitbl checks)
  #:use-module (ares suitbl core)
  #:use-module ((ares suitbl fixture) #:prefix fixture:))



(define-suite (compose-tests)
  (test "threads test context through fixtures from outer to inner" ()
    (define (make-context-enriching-fixture key)
      (lambda (context proceed)
        (proceed (acons key #t context) (lambda () #t))))

    (define combined-fixture
      (fixture:compose (make-context-enriching-fixture 'outer)
                       (make-context-enriching-fixture 'inner)))
    (define final-ctx
      (combined-fixture '((initial . #t)) (lambda (ctx _) ctx)))

    (is (equal? '(inner outer initial) (map car final-ctx))))

  (test "sets up outside-in and tears down inside-out" ()
    (define events '())
    (define (record! event)
      (set! events (cons event events)))
    (define outer
      (lambda (context proceed)
        (record! 'outer-setup)
        (proceed context (lambda () (record! 'outer-teardown)))))
    (define inner
      (lambda (context proceed)
        (record! 'inner-setup)
        (proceed context (lambda () (record! 'inner-teardown)))))

    ((fixture:compose outer inner)
     '()
     (lambda (context teardown!)
       (record! 'body)
       (teardown!)))

    (is (equal? '(outer-setup
                  inner-setup
                  body
                  inner-teardown
                  outer-teardown)
                (reverse events))))

  (test "runs outer teardown when inner teardown raises" ()
    (define outer-teardown-called? #f)
    (define outer
      (lambda (context proceed)
        (proceed context
                 (lambda ()
                   (set! outer-teardown-called? #t)))))
    (define inner
      (lambda (context proceed)
        (proceed context
                 (lambda ()
                   (error "inner teardown failed")))))

    (is (throws-exception?
         ((fixture:compose outer inner)
          '()
          (lambda (_ teardown!)
            (teardown!)))))
    (is outer-teardown-called?)))



(define-suite (fixture-wrap-with-tests)
  (test "wraps procedure in fixture dynamic extent" ()
    (define fixture-state (make-parameter 'outside))
    (define state-during-procedure #f)
    (define fixture
      (lambda (context proceed)
        (parameterize ((fixture-state 'inside))
          (proceed context (lambda () #t)))))

    ((fixture:wrap-with
      fixture
      (lambda (_)
        (set! state-during-procedure (fixture-state))))
     '())

    (is (eq? 'inside state-during-procedure)))

  (test "runs teardown when procedure raises" ()
    (define teardown-called? #f)
    (define fixture
      (lambda (context proceed)
        (proceed context
                 (lambda ()
                   (set! teardown-called? #t)))))

    (is (throws-exception?
         ((fixture:wrap-with
           fixture
           (lambda (_)
             (error "procedure failed")))
          '())))
    (is teardown-called?)))
