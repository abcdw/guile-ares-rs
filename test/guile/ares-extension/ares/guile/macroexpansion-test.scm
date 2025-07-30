;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2024 Andrew Tropin
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

(define-module (ares-extension ares guile macroexpansion-test)
  #:use-module (srfi srfi-64)
  ;; this require is needed for module-aware expansion test
  #:use-module (ice-9 match) ; don't remove it
  #:use-module (test-utils))

;; to avoid (ice-9 match) being unused module
(define my-match (match '() ('() 'hey)))

(define sync-macroexpand-op
  (@@ (ares-extension ares guile macroexpansion)
      sync-macroexpand-op))

(define (check-response test-name operation expected-value)
  (define response)

  (sync-macroexpand-op
   `((nrepl/message . ,operation)
     (reply! . ,(lambda (m) (set! response m)))))

  ;; (format #t "~s" response)
  (test-equal test-name
    expected-value
    response))

(define-test macroexpansion-ops-test
  (test-group "macroexpansion operations"
    (check-response
     "Simple when expansion"
     `(("op" . "ares.guile.macroexpansion/macroexpand")
       ("code" . "(when #t 'hi 'ho)"))
     `(("status" . #("done"))
       ("expansion" . "(if #t (begin 'hi 'ho))")))

    (check-response
     "Expansion of macro, which is not provided"
     `(("op" . "ares.guile.macroexpansion/macroexpand")
       ("code" . "(match '(a . b) ((k . v) k))"))
     `(("error" . "ice-9/psyntax.scm:2824:12: In procedure syntax-violation:\nSyntax error:\nunknown location: source expression failed to match any pattern in form (k . v)\n")
       ("status" . #("error" "macroexpand-error" "done"))))

    (check-response
     "Expansion of macro required from other module"
     `(("op" . "ares.guile.macroexpansion/macroexpand")
       ("code" . "(match '(a . b) ((k . v) k))")
       ("module" . "(ares-extension ares guile macroexpansion-test)"))
     `(("status" . #("done"))
       ("expansion" . ,(if (string<= (version) "3.0.9") ;different indentation rules
                           "(let* ((v '(a . b))\n       (failure\n         (lambda ()\n           ((@@ (ice-9 match) throw)\n            'match-error\n            \"match\"\n            \"no matching pattern\"\n            v)\n           #f)))\n  (if ((@@ (ice-9 match) pair?) v)\n    (let ((w ((@@ (ice-9 match) car) v))\n          (x ((@@ (ice-9 match) cdr) v)))\n      (let* ((k w) (v x)) k))\n    (failure)))"
                           "(let* ((v '(a . b))\n       (failure\n        (lambda ()\n          ((@@ (ice-9 match) throw)\n           'match-error\n           \"match\"\n           \"no matching pattern\"\n           v)\n          #f)))\n  (if ((@@ (ice-9 match) pair?) v)\n      (let ((w ((@@ (ice-9 match) car) v)) (x ((@@ (ice-9 match) cdr) v)))\n        (let* ((k w) (v x)) k))\n      (failure)))"))))

    (check-response
     "Unparsable code"
     `(("op" . "ares.guile.macroexpansion/macroexpand")
       ("code" . "]()"))
     `(("error" . "ice-9/read.scm:126:4: In procedure read-expr*:\n#<unknown port>:1:2: unexpected \"]\"\n")
       ("status" . #("error" "macroexpand-error" "done"))))

    (check-response
     "Missing code argument"
     `(("op" . "ares.guile.macroexpansion/macroexpand"))
     `(("error" . "@code{code} argument is required and must be a string.")
       ("status" . #("error" "macroexpand-error" "done"))))))
