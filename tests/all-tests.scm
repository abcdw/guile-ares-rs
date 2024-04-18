(define-module (all-tests)
  #:export (all-test-modules))

(define all-test-modules
  (map
   resolve-module
   '((bencode-test)
     (integration-test)
     (nrepl bootstrap-test)
     (nrepl server evaluation-test))))
