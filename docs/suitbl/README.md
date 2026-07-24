# suitbl quick guide

This note gives a short map of the suitbl testing library in this repository,
and the most common commands to run its tests.

## Library structure

suitbl implementation lives under `src/guile/ares/suitbl/`.

- `core.scm` - entry point for core API re-exports and `with-test-runner`.
- `checks.scm` - test-definition check helpers, including `throws-exception?`.
- `definitions.scm` - test DSL (`is`, `test`, `suite`, `define-suite`),
  current test-runner helpers, and test entity shapes.
- `exceptions.scm` - suitbl-specific exception types for DSL misuse and other
  structured failures.
- `runner.scm` - test runner implementation (`make-suitbl`).
- `state.scm` - runner state, loaded tests, suite forest, run history, summaries.
- `running.scm` - helpers related to running test and assertions.
- `reporters.scm` - output/reporter implementations and reporter combinators.
- `reporting.scm` - formatting helpers (tree rendering, locations, JUnit XML).
- `discovery.scm` - test module discovery and loading.
- `schedulers.scm` - test scheduling/filtering strategies.
- `presets.scm` - convenience presets that adjust runner config.
- `ares.scm` - project level helpers (`load-project-tests`, `run-tests`, etc).

## Definition syntax

Tests put the description before a context binding list. Use an empty list when
the body does not need the test context:

```scheme
(test "adds small numbers" ()
  (is (= 4 (+ 2 2))))
```

Bind the context by name when the body needs it:

```scheme
(test "uses its context" (ctx)
  (is (assoc-ref ctx 'expected)))
```

Named suites should use the parenthesized form:

```scheme
(define-suite (some-cool-tests)
  ...)
```

The old bare-name form, `(define-suite some-cool-tests ...)`, remains accepted
for compatibility and emits a deprecation warning.

A suite loader accepts optional metadata when called.  This metadata amends the
metadata declared by `suite-loader`, with call-time values taking precedence:

```scheme
(define load-tests
  (suite-loader "tests" 'metadata '((slow? . #t))
    ...))

(load-tests '((module . example)
              (slow? . #f)))
```

## Test structure

suitbl tests are mainly in:

- `tests/guile/ares/suitbl/*-test.scm`
- `tests/guile/ares/suitbl/specimens-test.scm` - reusable sample suites and
  tests for reporter inspection and runner testing.

Project integration for running suitbl subset is in:

- `tests/guile/suitbl-test-runner.scm`

## Running tests

From repository root:

- Run suitbl-focused test set:

  ```sh
  make check-suitbl
  ```

- Run all project tests:

  ```sh
  make check
  ```

- Produce JUnit XML output:

  ```sh
  make check-project-junit-output
  ```

## Maintenance notes

- When the suitbl module layout or responsibilities change, update this file in
  the same patch so the structure map stays accurate.
- When adding or removing suitbl tests, also bump the expected test count in
  `tests/guile/suitbl-test-runner.scm`.
