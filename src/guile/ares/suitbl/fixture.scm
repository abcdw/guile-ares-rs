;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl fixture)
  #:use-module ((ares suitbl exceptions)
                #:select
                (raise-suitbl-fixture-continuation-after-teardown-exception))
  #:use-module (ice-9 control))



#|
fixture is a function doing a setup

setup: ctx -> (values new-ctx tear-down) | (list new-ctx tear-down) | (alist ...)
tear-down: () -> #<unspecified>

What about no-tear-down fixtures?
(values new-ctx) (values new-ctx #f) (list new-ctx)?

What about parameterize and exception handlers? ctx -> (values
new-ctx) doesn't support this use case


Order of test/fixtures application: outer first, inner last?

fixtures/re-order-test-fixtures
fixtures/re-order-suite-fixtures

suite/fixture doesn't know anything about test
test/fixtures should be applied after suite/fixture
test/fixture has test info in context


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

(define (compose-fixtures f1 f2)
  (lambda (ctx f)
    (define ff
      (lambda (ctx1 teardown1)
        (define ff2
          (lambda (ctx2 teardown2)

            (define multi-teardown
              (lambda ()
                (teardown2)
                (teardown1)))
            (f ctx2 multi-teardown)))

        (f2 ctx1 ff2)))
    (f1 ctx ff)))

;; (reduce compose-fixtures (list 1 2 3))
;; reduce f1 f2 f3 -> f4


(define composed-fixture
  (fixture-compose fixture1 fixture2))

;; (run-test-with-fixture composed-fixture '() sample-test)

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

((k (lambda (ctx td) (sample-test ctx))))

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
;; (error "hi")
;; ((k (lambda (ctx td) td)))

#|
(lambda (f)
  (lambda (ctx)
    (define new-ctx (enchance-ctx ctx))
    (if I-don-t-like-fi
        (don-t-call-f)
        (f new-ctx))
    (tear-down! new-ctx)))

(f1 f2) -> f3

f = f1 f2 f3 + test
(f init-ctx)

f1 f2 -> ctx

f3 ctx -> ctx2

(test ctx2)

f4 ctx -> ctx3

(test7 ctx3)


((test/id)
 (test/suite-path 1 2)
 (test/body-procedure))

test1 1 2
test3 1 3 tear-down2 setup3

Fixture can be

`((fixture/set-up . ,lambda)
  (fixture/tear-down . ,huiambda))

or

a function returning ctx + tear-down lambda

- [ ] Do chain ctx for suite/fixtures (pass result of tear-down to
      next setup) or just pass a ctx produced by parent to new
      setup. There is a chance that second option is better because
      tear-down can forget to remove keys created by corresponding
      setup.
- [ ] suite-fixture/setup can return a ctx + tear-down closure

(())

fixture1 -> setup-costly-container 2s

(suite1
 'metadata
 (suite/fixtures fixture1) -> ctx1
 (test/fixtures fixture1)

 (suite2
  'metadata
  (suite/fixtures fixture2) ctx1 -> ctx2

  (test1 ...) -> (test1 ctx2) -costly op
  ) tear-down2

 (suite3
  'metadata
  (suite/fixtures fixture3) ctx1 -> ctx3
  (test2 ...) - costly op
  ) tear-down ctx3
 )

set-up1 init-ctx -> ctx1
set-up2 ctx1 -> ctx2

test ctx2

tear-down2 ctx2


set-up3 ctx1 -> ctx3

test2 ctx3

tear-down3 ctx3
tear-down1 ctx1


(chain init-ctx
       (setup1 _) -> ctx1
       (setup2 _) -> ctx2
       ;; (test1 ctx2)
       ;; (test2 ctx2)
       (tear-down2 ctx2)
       (setup3 ctx1)
       (test2 _)
       (tear-down3 _)
       (tear-down1 _))

https://github.com/day8/re-frame/blob/master/docs/Interceptors.md
|#
