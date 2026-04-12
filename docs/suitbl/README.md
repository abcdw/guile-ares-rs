# suitbl quick guide

This note gives a short map of the suitbl testing library in this repository,
and the most common commands to run its tests.

## Library structure

suitbl implementation lives under `src/guile/ares/suitbl/`.

- `core.scm` - entry point for core API re-exports and `with-test-runner`.
- `definitions.scm` - test DSL (`is`, `test`, `suite`, `define-suite`) and
  test entity shapes.
- `exceptions.scm` - suitbl-specific exception types for DSL misuse and other
  structured failures.
- `runners.scm` - test runner implementation (`make-suitbl-test-runner`).
- `state.scm` - runner state, loaded tests, suite forest, run history, summaries.
- `running.scm` - helpers related to running test and assertions.
- `reporters.scm` - output/reporter implementations and reporter combinators.
- `reporting.scm` - formatting helpers (tree rendering, locations, JUnit XML).
- `discovery.scm` - test module discovery and loading.
- `schedulers.scm` - test scheduling/filtering strategies.
- `presets.scm` - convenience presets that adjust runner config.
- `ares.scm` - project level helpers (`load-project-tests`, `run-tests`, etc).

## Test structure

suitbl tests are mainly in:

- `tests/guile/ares/suitbl/*-test.scm`

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
