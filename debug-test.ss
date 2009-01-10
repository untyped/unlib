(module debug-test mzscheme
  
  (require (lib "pregexp.ss"))
  
  (require (file "debug.ss")
           (file "test-base.ss"))
  
  (provide debug-tests)
  
  ; Utility procedures ---------------------------
  
  (define-syntax (capture-output stx)
    (syntax-case stx ()
      [(_ expr ...)
       #'(let ([out (open-output-string)]
               [thunk (lambda () expr ...)])
           (parameterize ([current-output-port out])
             (thunk)
             (get-output-string out)))]))
  
  ; Test suite -----------------------------------
  
  (define debug-tests
    (test-suite
     "All tests for debug"
     
     (test-case
      "Debug passes expression value through correctly"
      (capture-output 
       (check equal?
              (debug "Message" (+ 1 2 3))
              6)))
     
     (test-equal?
      "Debug prints message correctly"
      (capture-output
       (debug "Message" (+ 1 2 3)))
      (format "D,\"Message\",6~n"))
     
     (test-equal?
      "Debug does not print when debug-enabled? is set to #f"
      (with-debug-enabled? 
       #f
       (capture-output 
        (debug "Message" (+ 1 2 3))))
      "")
     
     (test-equal?
      "let-debug prints correctly"
      (capture-output 
       (let-debug ([a 1]
                   [b 2])
                  (+ a b)))
      "D,\"a\",1\nD,\"b\",2\n")
     
     (test-equal?
      "let*-debug support nesting"
      (capture-output 
       (let*-debug ([a 1]
                    [b (+ a 1)])
                   (+ a b)))
      "D,\"a\",1\nD,\"b\",2\n")
     
     (test-case
      "letrec-debug support mutual recursion"
      (check pregexp-match
             (pregexp (format "~a[0-9:]+~a[0-9:]+~a"
                              (pregexp-quote "D,\"odd?\",#<procedure:...ib/debug-test.ss:")
                              (pregexp-quote ">\nD,\"even?\",#<procedure:...ib/debug-test.ss:")
                              (pregexp-quote ">\n")))
             (capture-output 
              (letrec-debug ([odd?
                              (lambda (n)
                                (if (zero? n)
                                    #t
                                    (even? (sub1 n))))]
                             [even?
                              (lambda (n)
                                (if (zero? n)
                                    #f
                                    (odd? (sub1 n))))])
                            (odd? 7)))))
     
     (test-equal?
      "define-debug prints expected value"
      (capture-output
       (define-debug a 2)
       (void))
      "D,\"a\",2\n")
     
     ))
  
  )
