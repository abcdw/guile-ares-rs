;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl discovery-test)
  #:use-module (ares suitbl core)
  #:use-module (ares suitbl discovery))



(define-suite (test-file-path?-tests)
  (test ("matches only canonical Scheme test files" _)
    (is (test-file-path? "tests/guile/ares/suitbl/discovery-test.scm"))
    (is (test-file-path? "tests/guile/ares/suitbl/module-test.ss"))
    (is (not (test-file-path? "tests/guile/ares/suitbl/discovery-test.scm~")))
    (is (not (test-file-path? "tests/guile/ares/suitbl/discovery-test.scm.d/data.txt")))
    (is (not (test-file-path? "tests/guile/ares/suitbl/helper.scm")))
    (is (not (test-file-path? "tests/guile/ares/suitbl/discovery-test.scmx")))))

(define-suite (load-path-relative-file-path-tests)
  (test ("handles load path entries with and without trailing slash" _)
    (is (string=? "ares/foo-test.scm"
                  (load-path-relative-file-path
                   "tests/guile"
                   "tests/guile/ares/foo-test.scm")))
    (is (string=? "ares/foo-test.scm"
                  (load-path-relative-file-path
                   "tests/guile/"
                   "tests/guile/ares/foo-test.scm")))
    (is (string=? "ares/foo-test.scm"
                  (load-path-relative-file-path
                   "."
                   "./ares/foo-test.scm")))
    (is (string=? "ares/foo-test.scm"
                  (load-path-relative-file-path
                   "./"
                   "./ares/foo-test.scm")))))
