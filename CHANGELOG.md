# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

The long lines are not hard-wrapped.  Use softwrapping in your editor for reading.

## [Unreleased]

## [0.9.7] - 2026-04-23

### Added
- `docs/suitbl/README.md` documentation.
- `(ares guile exceptions)` module for displaying exceptions nicely.
- `(ares evaluation serialization)` module, extracted from thread-manager, with tests.
- `ares.logging/set-verbosity` nREPL operation for runtime verbosity control.
- Server dev mode: prints extensions and sets verbose logging by default.
- `wraps` extension property for middleware-style composition.
- `print-sorted-extensions` and `print-sorted-graph` extension helpers.
- Alist helpers, threading macro, and `exception->string` in prelude.
- `(ares guile alist)` module.
- Thread manager: evaluation queuing, interrupt operation, pure dynamic state.
- Fallback values for frame serialization failures.
- NLnet added to README Acknowledgements.
- `(ares suitbl)` framework restructured into separate modules: `core`, `definitions`, `discovery`, `reporters`, `reporting`, `running`, `runner`, `state`, `schedulers`, `presets`, `ares`, `specimens`, `exceptions`.
- SRFI-269 specification written and published.
- `ares-suitbl` CLI script with `--scheduler` and `--reporter` options.
- `(ares suitbl reporters)`: JUnit, dots, compact, tree, verbose, minimal, newline, dots-extended reporters and `reporter-every`/`reporter-first` combinators.
- `(ares suitbl schedulers)` module with `make-module`.
- `(ares suitbl presets)`: `raise-on-error`, `rerun-failed-or-all`, `make-scheduler:matching`.
- `(ares suitbl running)`: `with-exception-continuation` using delimited continuations, assertion-run/outcome tracking, run summaries.
- `(ares suitbl definitions)`: location information saved in definition macros.
- `(ares suitbl specimens)`: test specimens for demonstration.
- `(ares suitbl discovery)`: test discovery module.
- `(ares suitbl state)`: suite forest/tree support, scheduled-tests concept.
- `ares.testing` extension: `load-project-tests`, `load-module-tests`, `get-test-runner-stats`, test filtering operation.

### Changed
- Reworked evaluation thread manager.
- Exception handling moved to `nrepl.session` to ensure session/request IDs in error responses.
- `ares.scripts.ares` renamed to `ares-nrepl`.
- `test/` directory renamed to `tests/`.
- Logging: default verbosity set to quiet, logic moved to separate functions, wraps `#:*`.
- Extensions: `->dependency-graph` and `->providers` split into separate functions.
- Makefile: pure environment for reproducibility, `LANG` preserved, load-paths moved to variable, added `suitbl` and `clean-cache` targets.
- Updated to latest Guix
- Improved exception display in bencode, extensions, macroexpansion, and thread manager.
- Architecture section moved from README to a separate document.
- suitbl: `assert` renamed to `assertion` throughout.
- suitbl: `test-suite` renamed to `suite`.
- suitbl: `runners` renamed to `runner`.
- suitbl: reporter event types reorganized with `run/` and `load/` prefixes.
- `ares.testing` extension enabled by default.

### Fixed
- Multiple values handling in evaluation.
- Stack traces: cut unwanted frames from exception stacks.
- `stack->nrepl-value` only takes frames belonging to the stack.

### Removed
- SRFI-64 custom check targets from Makefile.
- Early stage of development warning from README.

## [0.9.6] - 2025-07-28

## [0.9.5] - 2024-05-27

## [0.9.4] - 2024-04-30

## [0.9.3] - 2024-03-14

## [0.9.1] - 2023-12-17

## [0.9.0] - 2023-12-04

[Unreleased]: https://git.sr.ht/~abcdw/guile-ares-rs
[0.9.7]: https://git.sr.ht/~abcdw/guile-ares-rs/refs/0.9.7
[0.9.6]: https://git.sr.ht/~abcdw/guile-ares-rs/refs/0.9.6
[0.9.5]: https://git.sr.ht/~abcdw/guile-ares-rs/refs/0.9.5
[0.9.4]: https://git.sr.ht/~abcdw/guile-ares-rs/refs/0.9.4
[0.9.3]: https://git.sr.ht/~abcdw/guile-ares-rs/refs/0.9.3
[0.9.1]: https://git.sr.ht/~abcdw/guile-ares-rs/refs/0.9.1
[0.9.0]: https://git.sr.ht/~abcdw/guile-ares-rs/refs/0.9.0
