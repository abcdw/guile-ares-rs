;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (ares file)
  #:export (search-in-load-path))

(define (search-in-load-path file)
  "Search @code{file} in @code{%load-path}, return absolute path or path
relative to current guile process."
  (define (absolute-path path)
    "Return absolute path to @code{path}, without resolving symlinks"
    (if (absolute-file-name? path)
        path
        (string-append (getcwd) "/" path)))

  (and=> (%search-load-path file)
         absolute-path))
