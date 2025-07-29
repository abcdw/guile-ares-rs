;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023, 2024 Andrew Tropin <andrew@trop.in>
;;;
;;; This file is part of guile-ares-rs.
;;;
;;; guile-ares-rs is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; guile-ares-rs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guile-ares-rs.  If not, see <http://www.gnu.org/licenses/>.

(define-module (ares extensions-test)
  #:use-module (ares guile)
  #:use-module (ares extensions)
  #:use-module (ares-extension ares bencode)
  #:use-module (ares-extension ares core)
  #:use-module (ares-extension ares extension)
  #:use-module (ares-extension ares logging)
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (test-utils))

(define-with-meta (ares.test handler)
  "Test extension that does nothing."
  `((provides . (ares.test))
    (requires . (ares.logging)))
  #nil)

(define base-extensions
  (list
   ares.core
   ares.bencode
   ares.logging
   ares.extension
   ares.test))

(define (extension-name ext)
  (procedure-property ext 'name))

(define (extension-names extensions)
  (map extension-name extensions))

(define-test extensions-format-test
  "Test runtime checks of extension structure"
  "hehe")

(define-test sort-extensions-test
  (test-group "Extensions sorted according to dependency definitions"
    (define sorted-extensions (sort-extensions base-extensions))
    (test-equal "Base extensions stack"
      '(ares.core ares.bencode ares.logging ares.extension ares.test)
      (extension-names sorted-extensions))

    (define sorted-reverse-extensions (sort-extensions (reverse base-extensions)))
    (test-equal "Reversed base extensions stack"
      '(ares.core ares.bencode ares.logging ares.extension ares.test)
      (extension-names sorted-reverse-extensions))))

(define (get-exception-message thunk)
    (catch
     #t
     thunk
     (lambda (key . args) (exception-message (car args)))))

(define-test exception-on-missing-dependency-test
  (define incomplete-stack
    (list
     ;; ares.core
     ares.bencode
     ares.logging
     ares.extension))

  (test-group "Incomplete extensions stack"
    (test-equal "Core extension missing"
      "There are no nodes providing @code{ares.core}, but \
@code{ares.bencode} requires it"
      (get-exception-message
       (lambda () (make-handler incomplete-stack))))))

(define-test get-operations-directory-test
  (test-equal "Operations information for base extensions is provided."
    '(("ares.extension/add-extension" . "Tries to add an extension to the list of extensions and rebuild\n@code{ares/handler}, if succeed the next and following iterations of\nthe loop will be using new handler.")
      ("ares.extension/describe" . "Provides a machine- and human-readable directory and documentation for\nthe operations supported by an nREPL endpoint.")
      ("describe" . "Provides a machine- and human-readable directory and documentation for\nthe operations supported by an nREPL endpoint.")
      ("ares.logging/set-verbosity" . "Set nREPL messages logging verbosity, set\n@code{ares.logging/verbosity} to @code{\"normal\"} to enable usual\nlogging and to anything else to disable it."))
    (get-operations-directory base-extensions)))

(define-test extension?-test
  (define test-extension
    (lambda (handler)
      "documentation"
      #((name . ares/test-extension)
        (requires . ())
        (provides . (ares/hues)))
      'hey))
  (extension? test-extension))

;; (use-modules ((nrepl bootstrap) #:prefix nrepl.bootstrap:))
;; (extension-names (sort-extensions nrepl.bootstrap:bootstrap-extensions))
;; Is transport a high-level entity or a part of extensions stack?
