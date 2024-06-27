(define-module (2024-06-24-test-error-exception-type.scm))

;; Helper macro to import a single private symbol from srfi-64
(define-syntax import-private-srfi-64
  (syntax-rules ()
    ((_ private-symbol)
     (define private-symbol (@@ (srfi srfi-64) private-symbol)))))

;; Main macro to import multiple private symbols from srfi-64
(define-syntax multiple-import-private-srfi-64
  (syntax-rules ()
    ((_ (sym ...))
     (begin
       (import-private-srfi-64 sym) ...))))

(multiple-import-private-srfi-64
 (%test-on-test-begin
  %test-on-test-end
  %test-source-line2
  %test-report-result))

(define-syntax %test-error
  (syntax-rules ()
    ((%test-error r etype expr)
     (cond ((%test-on-test-begin r)
            (let ((et etype))
              (test-result-set! r 'expected-error et)
              (%test-on-test-end
               r
               (catch #t
                      (lambda ()
                        (test-result-set! r 'actual-value expr)
                        #f)
                      (lambda (key . args)
                        (test-result-set! r 'actual-error
                                          (cons key args))

                        (define compound-exception-components
                          (record-accessor &compound-exception 'components))
                        (if (equal? '%exception key)
                            (any etype
                                 (compound-exception-components (car args)))
                            (equal? key etype)))))
              (%test-report-result)))))))

(define-syntax test-error
    (lambda (x)
      (syntax-case (list x (list (syntax quote) (%test-source-line2 x))) ()
	(((mac tname etype expr) line)
	 (syntax
	  (let* ((r (test-runner-get))
		 (name tname))
	    (test-result-alist! r (cons (cons 'test-name tname) line))
	    (%test-error r etype expr))))
	(((mac etype expr) line)
	 (syntax
	  (let* ((r (test-runner-get)))
	    (test-result-alist! r line)
	    (%test-error r etype expr))))
	(((mac expr) line)
	 (syntax
	  (let* ((r (test-runner-get)))
	    (test-result-alist! r line)
	    (%test-error r #t expr)))))))
;; (module-clear! (current-module))
