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
  #:use-module (ares extensions)
  #:use-module (ares ares-extensions bencode)
  #:use-module (ares ares-extensions extension)
  #:use-module (ares ares-extensions logger)
  #:use-module (ares ares-extensions core)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (test-utils))


(define base-extensions
  (list
   core-extension
   bencode-extension
   logger-extension
   extension-extension))

(define (extension-name ext)
  (assoc-ref ext 'name))

(define (extension-names extensions)
  (map extension-name extensions))

(define-test extensions-format-test
  "Test runtime checks of extension structure"
  "hehe")

(define-test sort-extensions-test
  (test-group "Extensions sorted according to dependency definitions"
    (define sorted-extensions (sort-extensions base-extensions))
    (test-equal "Base extensions stack"
      '("ares/core" "ares/bencode" "ares/logger" "ares/extension")
      (extension-names sorted-extensions))))

;; (use-modules ((nrepl bootstrap) #:prefix nrepl.bootstrap:))
;; (extension-names (sort-extensions nrepl.bootstrap:bootstrap-extensions))
;; Is transport a high-level entity or a part of extensions stack?
