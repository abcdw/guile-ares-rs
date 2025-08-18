((scheme-mode
  .
  ((indent-tabs-mode . nil)
   (eval . (put 'test-suite-thunk 'scheme-indent-function 1))
   (eval . (put 'test-suite 'scheme-indent-function 1))
   (eval . (put 'suite-thunk 'scheme-indent-function 1))
   (eval . (put 'suite 'scheme-indent-function 1))
   (eval . (put 'test-thunk 'scheme-indent-function 1))
   (eval . (put 'test 'scheme-indent-function 1))
   (eval . (put 'chain 'scheme-indent-function 1))
   (eval . (put 'chain-and 'scheme-indent-function 1))
   (eval . (put 'let/ec 'scheme-indent-function 1))
   ;; SRFI-64
   (eval . (put 'test-group 'scheme-indent-function 1))
   (eval . (put 'with-dynamic-state 'scheme-indent-function 1))
   (eval . (put 'with-fluids 'scheme-indent-function 1)))))
