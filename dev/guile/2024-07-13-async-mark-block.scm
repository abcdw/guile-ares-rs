(begin
  (use-modules (ice-9 threads))

  (define prompt-tag (make-prompt-tag 'tmp))

  (define (blocked-async-loop)
    (call-with-blocked-asyncs
     (lambda ()
       (let loop ((i 0))

         (format #t "\nstep: ~a\n" i)
         (format #t "--->\n")

         (call-with-prompt
          prompt-tag
          (lambda ()
            (format #t "++++ inside prompt\n" i)
            (call-with-unblocked-asyncs
             (lambda ()
               (format #t "++++ inside unblocked async\n" i)
               ;; This jump works fine.  Async get re-blocked
               (abort-to-prompt prompt-tag 'from-inside-loop))))
           (lambda (k . args)
             (format #t "==== jumped out of prompt: ~a\n" args)))

         (format #t "<---\n")

         (sleep 2)
         (when (< i 5)
           (loop (1+ i)))))))

  (let* ((th (call-with-new-thread (lambda () (blocked-async-loop)))))
    (sleep 3)
    (system-async-mark
     (lambda ()
       ;; This jump doesn't lead to re-blocking asyncs
       (abort-to-prompt prompt-tag 'from-async-mark))
     th)

    (sleep 8)))
