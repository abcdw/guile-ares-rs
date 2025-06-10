;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2023, 2024, 2025 Andrew Tropin <andrew@trop.in>
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

(define-module (ares evaluation)
  #:use-module (ares evaluation supervisor)
  #:use-module (ares evaluation thread)
  #:use-module (ares evaluation thread-manager)
  #:use-module (ares evaluation io)
  #:use-module (ares alist)
  #:use-module (ares ports)
  #:use-module (ares file)
  #:use-module (ares reflection modules)
  #:use-module (ares reusable-thread)
  #:use-module (ares guile)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (fibers io-wakeup)
  #:use-module (fibers operations)
  #:use-module (fibers scheduler)
  #:use-module (fibers timers)
  #:use-module (ice-9 control)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-197)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module ((system base compile) #:select (read-and-compile))
  #:use-module ((system repl debug) #:prefix repl-debug:)
  #:use-module ((system vm loader) #:select (load-thunk-from-memory))
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:re-export (evaluation-supervisor-thunk
               evaluation-supervisor-shutdown
               evaluation-supervisor-process-nrepl-message
               evaluation-thread-manager-thunk
               output-stream-manager-thunk))
