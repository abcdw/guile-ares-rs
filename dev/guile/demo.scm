
(define (comment)
  ;; 1. Simple REPL examples

  (display "hi")
  (values (+ 1 2) (+ 5 6))
  (current-module)

  ;; 2. Completion, eldoc, xref, go to module
  (use-modules (ice-9 control))
  (call-with-escape-continuation )

  ;; 3. Interrupt operation
  (let loop ((i 0))
    (format #t "hi: ~s\n" i)
    (usleep 100000)
    (loop (1+ i)))

  ;; 4. Continuations, evaluation queing, stdin/stderr
  (use-modules (ice-9 rdelim))

  (define kont #f)
  (let loop ((i 0)
             (previous-suffix 'default-suffix))
    (usleep 100000)
    (define suffix (call/cc (lambda (k) (set! kont k) previous-suffix)))
    (format #t "~s: ~s\n" i suffix)
    %load-path
    (when (equal? 'read-number suffix)
      (set! i (string->number (read-line)))
      (set! suffix 'after-we-finished-reading-from-stdin))
    (loop (1+ i) suffix))

  (kont 'hehe)
  (kont 'ohoho)
  (kont 'read-number)

  ;; Ways to support the project:
  ;; https://opencollective.com/rde
  ;; Tell about it to your friends and colleagues

  ;; Mailing list and issue tracker:
  ;; https://lists.sr.ht/~abcdw/rde-devel
  ;; https://todo.sr.ht/~abcdw/tickets
  )
