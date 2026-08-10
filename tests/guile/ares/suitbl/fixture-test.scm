;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl fixture-test)
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
                (reverse events)))))
