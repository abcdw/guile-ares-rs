(define-module (nrepl server)
  #:use-module ((ares server) #:prefix ares:)
  #:export (run-nrepl-server))

(define (run-nrepl-server . args)
  (format (current-error-port) "\
Warning: (nrepl server) module is deprecated, use (ares server) instead.
Warning: (nrepl server) module will be removed in 1.0 release.\n")
  (apply ares:run-nrepl-server args))
