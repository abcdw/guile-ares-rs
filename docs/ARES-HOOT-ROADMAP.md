takentaal v1.0

# (H)IDE for Guile Hoot

The goal of the project is to provide a high-quality development tooling (IDE, parsing and reflection libraries, nREPL) for Guile Hoot and make that tooling reusable across different ecosystems and text editors. Good development tooling is important to make technologies like Guix, Spritely Goblins, other Guile-based projects (and their WebAssembly counterparts) more accessible, and the development of them (and their ecosystems) more efficient and enjoyable. The reusability is achieved by using standardized protocols and technologies like nREPL and tree-sitter. The enjoyment and efficiency achieved by thorough and careful design and implementation.

We plan to work on common Guile tasks and features first, Hoot specific second. This way we can immediately use the produced work with guile project, including Ares itself.

## {8000} Syntax Tools and Formatter

Set of tools for operating on the source code of the language. It will open the gates for reliable and portable syntax highlighting, source code formatting, linting, selection, search and structural navigation.

This set of tools will be based on tree sitter, the most wide-spread grammar definition format supported by multiple text editor, IDEs and CLIs.

https://github.com/6cdh/tree-sitter-scheme

- {1000} Setup Tree Sitter Introspection, Test and Dev Infra
- {1000} Implement Guile Scheme tree-sitter grammar
- {1000} Granular ts queries needed for formatter/scheme-ts-mode
- {1000} Implement minimal ts-based source code formatter
- {3000} Implement formatting rules engine
- {1000} Documentation, release as a guix package

## {9000} Arei (Emacs Frontend) Migration to Tree Sitter

As a reference implementation for an IDE's frontend we use Arei, an emacs-lisp package. This milestone focuses on moving all the syntax related logic from emacs lisp to tree sitter, so we can update tree-sitter tools instead of doing on adhoc implementations for each frontend.

https://tonsky.me/blog/syntax-highlighting/

As a verification for flexibility/extensibility of our grammar, we will provide a new project-specific syntax reader extensions for guile and corresponding tree sitter queries and rules.

- {1000} Introduce testing infrastructure to Arei
- {1000} Minimal scheme-ts-mode
- {3000} font-lock, comments, imenu, indentation
- {1000} Using ts for selecting/extracting code in Arei
- {2000} PoC Persistent Data Structures and Reader Literals for them
- {1000} Guix package and Arei Release

## {9000} Stepping Macroexpander

One of the hardest things in Scheme is to grasp, what is going on during macro expansion, this set of tools will allow to visually track the macro expansion process looking at every step of the expansion. This is one of the most requested features by university professors, who teach their courses in Scheme. Also, as a part of this milestone we make RnD and provide a library for F-expressions, more flexible and runtime friendly alternative to macros. It can spark the further development of the Scheme ecosystem and minimize the problems associated with macros usage.

https://github.com/emacsorphanage/macrostep

- {2000} Initial Stepping Macroexpander and API
- {2000} Production Grade Macroexpander
- {2000} Arei Integration
- {3000} PoC F-expressions Implementation

## {9000} Guile External Process lib

Workflows automation, integration of different CLIs and APIs into coherent pipelines requeres spawning multiple processes, controlling and processing their output and execution. To call formatters/linters/parsers and combine/process their output we need a robust and convenient tooling for that. Unfortunately, current system*, (ice-9 popen) are not capable for that, so we need to implement a library for that.

https://github.com/babashka/process

- {2000} A simple lib for running and managing external process
- {1000} Handling stderr/stdout
- {2000} Propogation Networks PoC for Reactive Communication
- {1000} Pipelines
- {3000} Handling Edge Cases

## {3500} Ares-Hoot Preparation

Preparing testing infrastructrure and migrating Ares to multiple runtimes/compilation targets.

- {500} Split out src/suitbl src/vendored-deps out of primary ares code.
- {2000} Port SRFI-269/suitbl test runner to Hoot.
- {1000} Setup WASI infrastructure for tests.

## {8000} Context-aware Evaluation

Design and implementation of contex-aware evaluation. There are multiple contexts, where we want to evaluate a particular scheme expression at point (inside exception context, either hoot or guile, inside goblin's VAT, fibers scheduler, or wherever else). We need to provide an understandable and reliable interface for users and corresponding infrastructure for IDE frontend developers.

Nested REPLs can be implemented with continuations and restarts. See MIT Scheme restarts. https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Condition-Handling.html

- {1000} Write use cases (hoot/guile, execption, debbugger, fibers, goblins)
- {2000} Design and prototype UI
- {2000} Design underlying data layer and state machine
- {3000} Switch Ares/Arei to new context-aware evaluation UI

## {5000} Ares-Hoot Implementation

The final wiring of all provided tools and infrastructure to the hoot runtime via WebSocket+nREPL and release.

- {1000} nREPL and base nREPL ares-extensions
- {2000} Advanced extension (macroexpansion, logging, etc)
- {1000} PoC VSCodium extension
- {1000} Guix package and Release
