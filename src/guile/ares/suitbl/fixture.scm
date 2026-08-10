;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl fixture)
  #:use-module ((ares suitbl exceptions)
                #:select
                (raise-suitbl-fixture-continuation-after-teardown-exception))
  #:use-module (ice-9 control)
  #:export (compose))



#|


## Fixtures

fixture is a function accepting a test context, optionally modifying
dynamic state, enriching the context and calling proceed function with
new-context and teardown! in updated dynamic extent.

fixture: context × proceed -> any
proceed: new-context × teardown! -> any
teardown!: () -> unspecified


## Adding fixtures to tests.

suite/fixture doesn't know anything about test
test/fixtures should be applied after suite/fixture
test/fixture has test info in context

## Fixture application order.
Order of test/fixtures application: outer first, inner last?

fixtures/re-order-test-fixtures
fixtures/re-order-suite-fixtures

## Context contsruction.

ctx : suite-fixture/ctx + test/ctx + test-fixture/ctx
test/fixture can enrich/use/override test context.

|#


(define (fixture1 f)
  (lambda (ctx)
    (define new-ctx)
    (setup!)
    (parameterize ((a 'ha))
      (f new-ctx))
    (teardown!)))

(define (run-test-with-fixture fixt init-ctx test)
  (define tr (lambda (ctx teardown!)
               (test ctx)
               (teardown!)))

  (fixt init-ctx tr))

(define (compose outer-fixture inner-fixture)
  "Compose OUTER-FIXTURE and INNER-FIXTURE into one fixture."
  (lambda (context proceed)
    (outer-fixture
     context
     (lambda (outer-context outer-teardown!)
       (inner-fixture
        outer-context
        (lambda (inner-context inner-teardown!)
          (proceed
           inner-context
           (lambda ()
             (inner-teardown!)
             (outer-teardown!)))))))))

(define a (make-parameter 'a))
(define b (make-parameter 'b))
(define tmp 1)

(define simple-fixture-a
  (lambda (ctx f)
    (parameterize ((a 'hello))
      (set! tmp (1+ tmp))
      (define teardown (lambda ()
                         (format #t "teaaaardown!")
                         (set! tmp (1- tmp))))
      (define new-ctx (acons 'a 'hello ctx))
      (f new-ctx teardown))))

(define simple-fixture-b
  (lambda (ctx f)
    (parameterize ((b 'hello))
      (define teardown (lambda () 'hi))
      (define new-ctx (acons 'b 'hoho ctx))
      (f new-ctx teardown))))

(define initial-ctx
  '((nothing . interesting)))

(define (sample-test ctx)
  (format #t "ctx: ~a\na: ~a\nb: ~a\n" ctx (a) (b)))

;; (parameterize ((a 'ha))
;;   (sample-test '(ho)))

(define (fixture->continuation fixture initial-context)
  (define teardown-called? #f)
  (let ((prompt-tag (make-prompt-tag "fixture")))
    (call-with-prompt
        prompt-tag
      (lambda ()
        (fixture
         initial-context
         (lambda (ctx teardown)
           ((abort-to-prompt prompt-tag)
            ctx
            (lambda ()
              (set! teardown-called? #t)
              (teardown))))))
      (lambda (continuation)
        (lambda arguments
          (if teardown-called?
              (raise-suitbl-fixture-continuation-after-teardown-exception)
              (apply continuation arguments)))))))

(define k
  (fixture->continuation simple-fixture-a initial-ctx))

;; ((k (lambda (ctx td) (sample-test ctx))))

(define (compose-fixture-continuation continuation fixture)
  (let ((prompt-tag (make-prompt-tag "composed-fixture")))
    (call-with-prompt
        prompt-tag
      (lambda ()
        (continuation
         (lambda (ctx1 teardown1)
           (fixture
            ctx1
            (lambda (ctx2 teardown2)
              ((abort-to-prompt prompt-tag)
               ctx2
               (lambda ()
                 (teardown2))))))))
      (lambda (composed-continuation)
        composed-continuation))))

(define continuation-ab
  (compose-fixture-continuation
   k
   simple-fixture-b))

(continuation-ab (lambda (ctx td) (sample-test ctx)))
((continuation-ab (lambda (ctx td) td)))
((k (lambda (ctx td) td)))

tmp
;; ((k (lambda (ctx td) td)))
