;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2025 Libre en Communs <contact@a-lec.org>
;;; Copyright © 2025 Noé Lopez <noelopez@free.fr>
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

(define-module (ares meta-commands)
  #:use-module ((system repl common) #:select (make-repl))
  #:export (*command-infos*
            call-meta-command))

(define *command-infos* (@@ (system repl command) *command-infos*))

(define (call-meta-command name arguments)
  "Calls a meta-command named NAME with ARGUMENTS, a string that will be
parsed with the meta-command’s arguments reader."
  (let* ((info (hashq-ref *command-infos* (string->symbol name)))
         (proc (car info))
         (arguments-reader (cdr info))
         (repl (make-repl (current-language)))
         (args (with-input-from-string arguments
                 (lambda () (arguments-reader repl)))))
    (if args
        (apply proc repl args)
        ;; This will be shown as the result of meta-command, and there
        ;; will be an actual error message in stdout, written by the
        ;; arguments reader.
        'interrupted)))
