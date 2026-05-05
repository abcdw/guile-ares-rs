;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl discovery-test)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl discovery))



(define-suite test-file-path?-tests
  (test "matches only canonical Scheme test files"
    (is (test-file-path? "tests/guile/ares/suitbl/discovery-test.scm"))
    (is (test-file-path? "tests/guile/ares/suitbl/module-test.ss"))
    (is (not (test-file-path? "tests/guile/ares/suitbl/discovery-test.scm~")))
    (is (not (test-file-path? "tests/guile/ares/suitbl/discovery-test.scm.d/data.txt")))
    (is (not (test-file-path? "tests/guile/ares/suitbl/helper.scm")))
    (is (not (test-file-path? "tests/guile/ares/suitbl/discovery-test.scmx")))))
