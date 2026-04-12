# Guile function inlining

Practical note for avoiding function inlining and preserving stack frames at `-O2`.

Use these two tricks:

1. Do not rely on plain local `define` for helper procedures. It may be inlined away.
2. If you need caller frames too, keep the call out of tail position.

Recommended pattern:

```scheme
(lambda ()
  (define helper #f)
  (set! helper
        (lambda ()
          (error "boom")))
  (list (helper)))
```

Why:

- This mutable local binding prevented helper inlining in the experiment.
- Wrapping the call with `(list ...)` kept the caller frame from being removed by tail call optimization.

Avoid relying on this shape when you need stack frames:

```scheme
(lambda ()
  (define (helper)
    (error "boom"))
  (helper))
```

In the experiment, Guile could inline `helper`, so its frame disappeared.

```scheme
(define top #f)
(define mid #f)
(define bot #f)

(set! bot (lambda () (error "boom")))
(set! mid (lambda () (list (bot))))
(set! top (lambda () (list (mid))))
```

That keeps the expected frames visible in stack.

Experiment source:

- `dev/guile/guile-experiments/function-inlining.scm`

Caveat:

- This is an empirical compiler behavior, not a documented `noinline` feature.
