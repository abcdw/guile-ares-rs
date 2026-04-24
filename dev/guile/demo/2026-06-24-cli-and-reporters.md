# suitbl CLI

## Demo

## Intro
SRFI-269

## Installing
guix time-machine -- shell guile guile-ares-rs

## Running
ares-suitbl

## Running with custom reporters
ares-suitbl -r compact
ares-suitbl -r silent
ares-suitbl -r "(lambda (m) (format #t \"~a\\n\" (assoc-ref m 'type)))"

'(@ (your module) your-report)'

Load -> Schedule -> Run

## Load Paths and guile args
export GUILE_LOAD_PATH=src/guile:tests/guile
ares-suitbl -r silent

## Supressing Guile's annoying warnings
ares-suitbl -r silent 2> /dev/null

## Building report we want

- load-tree :: load overview
- run-dots :: compact run overview
- run-dots-extended :: extended outcomes for better understanding of the run
- run-plan-compact :: what is scheduled
- run-summary :: run results
- verbose-noly-failed :: failure reports
ares-suitbl -r "(lambda (m) (format #t \"~a\\n\" (assoc-ref m 'type)))"

## Combinining Reporters
ares-suitbl -r '(reporter-every (list load-tree run-dots run-summary))'

reporter-first

## Scheduler/Selector
ares-suitbl -s '(make-matching "macro")'
ares-suitbl -s '(make-matching "macro")' -r compact
ares-suitbl -s '(make-matching "macro")' -r junit
ares-suitbl -s '(make-matching "macro")' -r base-all

'(@ (your module) your-scheduler)'

## Extended test outcomes
Legend: .=pass, Z=zero assertions, F=fail, E=error, A=aborted

## We may break API soon
both definition and running (reporters, schedulers)

## Conclusion
We make probably the best testing library, so try it out
