# suitbl quick guide

This note gives a short map of the suitbl testing library in this repository,
and the most common commands to run its tests.

## Library structure

suitbl implementation lives under `src/guile/ares/suitbl/`.

- `core.scm` - entry point for core API re-exports and `with-test-runner`.
- `checks.scm` - test-definition check helpers, including `throws-exception?`.
- `definitions.scm` - test DSL (`is`, `testing`, `test`, `suite`,
  `define-suite`), current test-runner helpers, and test entity shapes.
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

Suite and test loaders accept optional metadata when called.  They
send it in the `load/metadata` field of the load message, separately
from metadata originally declared.  The standard runner merges both
when it loads the entity, with call-time values taking precedence:

```scheme
(define load-suite
  (suite-loader "tests"
    (metadata '((slow? . #t)))
    ...))

(define load-test
  (test-loader "works" ()
    (metadata '((slow? . #t)))
    ...))

(load-suite '((module . example)
              (slow? . #f)))
(load-test '((slow? . #f)))
```

## Assertion context

Use `testing` to give related assertions shared human-readable context:

```scheme
(test "permission inheritance" ()
  (testing "administrator"
    (testing "project permissions"
      (is (member 'project/write permissions)))))
```

Each `is` captures the descriptions of its dynamically enclosing `testing`
forms under `assertion/context`, ordered from outermost to innermost.  An
assertion outside `testing` has an empty context.  The assertion body restores
its captured context when run, so nested assertions keep the context even when
a runner defers execution.

`testing` sends no runner message.  It evaluates its description once and
returns the values produced by its last body form.

## Captured test output

The standard runner captures `current-output-port` and `current-error-port`
while each test runs.  Capture covers the test's fixtures and body.  The
resulting strings are retained in its test-run record as `test-run/stdout` and
`test-run/stderr`, so test output does not interfere with progress reporting.

Human-readable verbose reports print only non-empty captured streams.  The
`base` and `compact` reporters show these reports for failed and erroring tests,
while `base-all` shows them for every test.  The JUnit reporter writes non-empty
streams as `system-out` and `system-err` elements.

## Suite and test IDs

The standard `make-suitbl` runner assigns a `suite/id` to each suite
load instance and a `test/id` to each test load instance.  IDs are
unique for the lifetime of one runner, so calling the same suite or
test loader twice produces two different IDs.  Suite and test ID sets
are independent and can contain same values.  The IDs are not
persistent and may be reused by a different runner or process.

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
