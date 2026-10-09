;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl fixture)
  #:use-module ((ares suitbl exceptions)
                #:select
                (raise-suitbl-fixture-continuation-after-teardown-exception))
  #:use-module (ice-9 control)
  #:export (compose
            wrap-with))



#|


## Fixtures

A fixture accepts a test context and a proceed procedure.  It may
modify dynamic state, set up needed resources, and enrich the context,
then call the proceed procedure with the updated context and a teardown
procedure while that dynamic state is active.

A runner may suspend a fixture at its call to proceed and re-enter its
dynamic extent multiple times.  Exiting that extent therefore does not
necessarily mean that the fixture is finished.  Final cleanup for
successfully established resources must be placed in the teardown
procedure passed to proceed.

The runner is responsible for invoking teardown and handling
exceptions from proceed.  Fixtures should let such exceptions
propagate rather than guard against them or attempt final cleanup
themselves.

fixture: context × proceed -> any
proceed: context × teardown! -> any
teardown!: () -> unspecified
compose: fixture × fixture -> fixture
procedure1: any -> any
wrap-with: fixture × procedure1 -> procedure1

## Fixture activation
activation: proceed -> any
activate: fixture × context -> activation
extend: activation × fixture -> activation
run-in: activation × procedure1 -> any
deactivate!: actvation -> unspecified

call-with-activation fixture × context × procedure1 -> unspecified
call-with-extension activation × fixture × procedure1 -> unspecified

with-activation
with-extension

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

(define (wrap-with fixture procedure)
  "Return a procedure that accept a context, runs PROCEDURE inside
FIXTURE's dynamic extent and calls FIXTURE's teardown."
  (lambda (initial-context)
    (fixture
     initial-context
     (lambda (context teardown!)
       (dynamic-wind
         (lambda () #t)
         (lambda () (procedure context))
         teardown!)))))

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
             (dynamic-wind
               (lambda () #t)
               inner-teardown!
               outer-teardown!)))))))))

(define (activate fixture initial-context)
  "Suspend FIXTURE at its proceed call and return an activation procedure.
Calling the activation re-enters FIXTURE's dynamic extent.  Teardown runs only
when the supplied proceed procedure explicitly invokes its teardown argument."
  (define teardown-called? #f)
  (let ((activation-prompt-tag (make-prompt-tag "fixture activation")))
    (call-with-prompt activation-prompt-tag
      (lambda ()
        (fixture
         initial-context
         (lambda (context teardown!)
           ((abort-to-prompt activation-prompt-tag)
            context
            (lambda ()
              (set! teardown-called? #t)
              (teardown!))))))
      (lambda (continuation)
        (lambda (proceed)
          (if teardown-called?
              (raise-suitbl-fixture-continuation-after-teardown-exception)
              (continuation proceed)))))))

(define (comment)
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

  (define k
    (activate simple-fixture-a initial-ctx))

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
  )
