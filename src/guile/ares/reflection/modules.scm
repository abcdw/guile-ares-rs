;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2024, 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares reflection modules)
  #:use-module (ares file)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:export (all-modules
            module-filename
            string->resolved-module))

(define (submodules mod)
  "Return a list of submodules."
  (hash-map->list (lambda (k v) v) (module-submodules mod)))

(define (root-modules)
  "Return a list of submodules of '() module."
  (submodules (resolve-module '() #f)))

(define* (all-child-modules mod #:optional (seen '()))
  "Recursively traverse all the submodules of MOD and build a list out of
it."
  (let ((mod-submodules
         (filter (lambda (m) (not (member m seen))) (submodules mod))))
    (fold (lambda (m all) (append (all-child-modules m all) all))
          (list mod)
          mod-submodules)))

(define (all-modules)
  "Return a list of all modules."
  (resolve-module '(boot-9))
  (let* ((roots (root-modules))
         (children (append-map all-child-modules roots)))
    children))

(define (modules-with-module-kind)
  "Return a list of all modules, having a module-kind."
  (define (module-with-module-kind m)
    (and (module-kind m) m))
  (let* ((guile-module (resolve-module '(guile)))
         (roots (root-modules))
         (children (append-map all-child-modules roots)))
    (filter-map module-with-module-kind children)))

(define (module-filename mod)
  "Return a path to module if corresponding file found in %load-path."
  (let ((name-parts
         (cond ((equal? '(boot-9) (module-name mod)) '("ice-9" "boot-9"))
               ((equal? '(guile) (module-name mod)) '("ice-9" "boot-9"))
               (else (map symbol->string (module-name mod))))))
    (search-in-load-path (string-join name-parts "/"))))

(define (string->resolved-module str)
  "Try to resolve the Guile module or R7RS library named by STR."
  (and (string? str)
       (and-let* ((name (with-input-from-string str read))
                  (interface
                   (false-if-exception
                    (resolve-r6rs-interface name))))
         ;; The R6RS resolver implements R7RS SRFI name translation, but
         ;; returns an interface.  Evaluation needs the underlying module.
         (resolve-module (module-name interface) #:ensure #f))))

;; https://git.sr.ht/~whereiseveryone/toys/tree/master/item/toys/discovery.scm
;; (scheme-modules)
