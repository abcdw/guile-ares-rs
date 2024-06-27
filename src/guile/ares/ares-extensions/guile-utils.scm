;;; guile-ares-rs --- Asynchronous Reliable Extensible Sleek RPC Server
;;;
;;; Copyright © 2024 Nikita Domnitskii
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

(define-module (ares ares-extensions guile-utils)
  #:use-module (ares guile)
  #:export (ares/guile-utils))

(define (get-load-path context)
  (let ((reply! (assoc-ref context 'reply!))
        (load-path (map (lambda (path)
                          (or (false-if-exception
                               (canonicalize-path path))
                              path))
                        %load-path)))
    (reply!
     `(("status" . #("done"))
       ("load-path" . ,(list->vector load-path))))))

(define operations
  `(("ares.guile/load-path" . ,get-load-path)
    ("ares/load-path" . ,get-load-path)))

;; It should be clear that it provides utilities related to guile, not
;; hoot and all operations are prefixed with ares.guile/
(define-with-meta (ares/guile-utils handler)
  "Handles load-path related functionality."
  `((provides . (ares.guile/utils))
    (requires . (nrepl/session))
    (handles . ,operations))
  (lambda (context)
    (let* ((message (assoc-ref context 'nrepl/message))
           (op (assoc-ref message "op"))
           (operation-function (assoc-ref operations op)))
      (if operation-function
          (operation-function context)
          (handler context)))))
