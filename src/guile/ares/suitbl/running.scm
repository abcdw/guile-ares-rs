;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl running)
  #:use-module ((srfi srfi-1) #:select (count))
  #:use-module ((ice-9 control) #:select (abort-to-prompt
                                          call-with-prompt
                                          make-prompt-tag))
  #:use-module ((ice-9 exceptions) #:select (raise-exception
                                             with-exception-handler))
  #:export (returned?
            returned-value
            raised?
            raised-exception
            raised-continuation
            with-exception-continuation
            make-assertion-run
            make-test-run
            assertion-run->reporter-message
            assertion-run-result->assertion-outcome
            assertion-run-result->reporter-message
            assertion-runs->assertion-summary
            assertion-outcomes->assertion-summary
            assertion-summary->test-run-summary
            assertion-summary->test-run-outcome
            run-summary->run-outcome
            assertion-runs->test-run-summary
            assertion-outcomes->test-run-summary))


;;;
;;; Running helpers
;;;

(define (returned? x)
  "Return true when X is a successful run result."
  (and (pair? x)
       (eq? 'returned (car x))))

(define (returned-value returned)
  "Extract the value from RETURNED run result."
  (cdr returned))

(define (raised? x)
  "Return true when X is a failed run result carrying an exception."
  (and (pair? x)
       (eq? 'raised (car x))))

(define (raised-exception raised)
  "Extract the exception object from RAISED run result."
  (assoc-ref (cdr raised) 'exception))

(define (raised-continuation raised)
  "Extract the replay continuation from RAISED run result."
  (assoc-ref (cdr raised) 'continuation))

(define (with-exception-continuation thunk)
  "Run THUNK and return a tagged result.

Returns:
- (returned . RESULT), when THUNK succeeds.
- (raised
    (exception . EXCEPTION)
    (continuation . K)), when THUNK raises an exception,
  where K rewinds to the protected call and replays THUNK until
  EXCEPTION is raised again in the current dynamic context."
  (let ((exception-continuation-tag
         (make-prompt-tag "suitbl-exception-continuation")))
    (define (run replaying-exception?)
      (with-exception-handler
       (lambda (exception)
         (if replaying-exception?
             (raise-exception exception)
             (abort-to-prompt exception-continuation-tag exception)))
       (lambda ()
         (cons 'returned (thunk)))
       #:unwind? #f))

    (call-with-prompt
     exception-continuation-tag
     (lambda () (run #f))
     (lambda (_ exception)
       `(raised
         (exception . ,exception)
         (continuation . ,(lambda () (run #t))))))))


;;;
;;; Assertion run helpers
;;;

(define (make-assertion-run assertion run-result)
  "Build an assertion run record from ASSERTION and RUN-RESULT."
  `((assertion . ,assertion)
    (assertion-run/result . ,run-result)
    (assertion-run/outcome
     . ,(assertion-run-result->assertion-outcome run-result))))

(define (assertion-run-result->assertion-outcome run-result)
  "Classify RUN-RESULT as one of: pass, fail, or error."
  (cond
   ((returned? run-result)
    (if (returned-value run-result) 'pass 'fail))
   ((raised? run-result)
    'error)
   (else
    #f)))

(define (assertion-run-result->reporter-message run-result)
  "Convert RUN-RESULT to an assertion reporter message fragment."
  (and (assertion-run-result->assertion-outcome run-result)
       '((type . run/assertion-end))))

(define (assertion-run->reporter-message assertion-run)
  "Convert ASSERTION-RUN to a complete reporter message."
  (let ((assertion (assoc-ref assertion-run 'assertion))
        (run-result (assoc-ref assertion-run 'assertion-run/result)))
    (and (assertion-run-result->reporter-message run-result)
         `((type . run/assertion-end)
           (assertion . ,assertion)
           (assertion-run . ,assertion-run)))))

(define (assertion-run-outcome assertion-run)
  (assoc-ref assertion-run 'assertion-run/outcome))

(define (assertion-runs->assertion-summary assertion-runs)
  "Summarize ASSERTION-RUNS into pass, failure, and error counts."
  (assertion-outcomes->assertion-summary
   (map assertion-run-outcome assertion-runs)))

(define (assertion-outcomes->assertion-summary outcomes)
  "Summarize assertion OUTCOMES into pass, failure, and error counts."
  `((passes . ,(count (lambda (x) (eq? x 'pass)) outcomes))
    (failures . ,(count (lambda (x) (eq? x 'fail)) outcomes))
    (errors . ,(count (lambda (x) (eq? x 'error)) outcomes))
    (assertions . ,(length outcomes))))


;;;
;;; Run summary helpers
;;;

(define (run-summary->run-outcome run-summary)
  "Convert RUN-SUMMARY alist into a run outcome symbol.

Returns one of: 'pass, 'fail, or 'error.  If both failures and errors
are present, run outcome is considered 'error."
  (let ((error? (> (assoc-ref run-summary 'errors) 0))
        (fail? (> (assoc-ref run-summary 'failures) 0)))
    (cond
     (error? 'error)
     (fail? 'fail)
     (else 'pass))))


;;;
;;; Test run helpers
;;;

(define (assertion-summary->test-run-summary assertion-summary)
  "Convert ASSERTION-SUMMARY into a per-test run summary alist."
  (let ((error? (> (assoc-ref assertion-summary 'errors) 0))
        (fail? (> (assoc-ref assertion-summary 'failures) 0)))
    `((tests . 1)
      (failures . ,(if (and fail? (not error?)) 1 0))
      (errors . ,(if error? 1 0))
      (skipped . 0)
      (assertions . ,(assoc-ref assertion-summary 'assertions)))))

(define (assertion-summary->test-run-outcome assertion-summary)
  "Convert ASSERTION-SUMMARY alist into a test run outcome symbol."
  (run-summary->run-outcome
   (assertion-summary->test-run-summary assertion-summary)))

(define (assertion-runs->test-run-summary assertion-runs)
  "Convert ASSERTION-RUNS into a per-test run summary alist."
  (assertion-summary->test-run-summary
   (assertion-runs->assertion-summary assertion-runs)))

(define (assertion-outcomes->test-run-summary outcomes)
  "Convert assertion OUTCOMES into a per-test run summary alist."
  (assertion-summary->test-run-summary
   (assertion-outcomes->assertion-summary outcomes)))

(define (make-test-run test test-run-result assertion-runs)
  "Build a test run record from TEST, TEST-RUN-RESULT, and ASSERTION-RUNS."
  (let* ((assertion-summary
          (assertion-runs->assertion-summary assertion-runs))
         (test-run-summary
          (assertion-summary->test-run-summary assertion-summary))
         (test-run-outcome
          (assertion-summary->test-run-outcome assertion-summary)))
    `((test . ,test)
      (test-run/result . ,test-run-result)
      (test-run/assertion-runs . ,assertion-runs)
      (test-run/summary . ,test-run-summary)
      (test-run/outcome . ,test-run-outcome))))
