(module trace-test mzscheme
  
  (require (lib "etc.ss"))
  
  (require (file "trace.ss")
           (file "test-base.ss"))
  
  (provide trace-tests)
  
  ; Utility procedures ---------------------------
  
  (define-syntax (capture-output stx)
    (syntax-case stx ()
      [(_ expr ...)
       #'(let ([out (open-output-string)]
               [thunk (lambda () expr ...)])
           (parameterize ([current-output-port out])
             (thunk)
             (get-output-string out)))]))
  
  ; Test data ------------------------------------
  
  (define-traced (foo a b) (+ a b))
  (define-traced bar (lambda (a b) (+ a b)))
  (define-traced baz (opt-lambda ([a 1] [b 2]) (+ a b)))
  
  (define quux (lambda-traced (a b) (+ a b)))
  
  ; Test suite -----------------------------------
  
  (define trace-tests
    (test-suite
     "All tests for trace"
     
     (test-case
      "define-traced function prints entry and exit"
      (check
       string=?
       (capture-output (foo 1 2))
       "> (foo 1 2)\n< (foo 1 2)\n"))
     
     (test-case
      "define-traced lambda prints entry and exit"
      (check
       string=?
       (capture-output (bar 1 2))
       "> (bar 1 2)\n< (bar 1 2)\n"))
     
     (test-case
      "define-traced opt-lambda prints entry and exit"
      (check
       string=?
       (capture-output (baz))
       "> (baz 1* 2*)\n< (baz 1* 2*)\n"))
     
     (test-case
      "lambda-traced prints entry and exit"
      (check
       string=?
       (capture-output (quux 1 2))
       "D,\"Entering traced lambda\",(1 2)\nD,\"Leaving traced lambda\",3\n"))
     ))
  )