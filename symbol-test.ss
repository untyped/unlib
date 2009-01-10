(module symbol-test mzscheme
    
  (require (file "symbol.ss")
           (file "test-base.ss"))
  
  (provide symbol-tests)
  
  (define symbol-tests
    (test-suite
     "symbol.ss"
     
     (test-equal?
      "Symbol append works"
      (symbol-append 'a- 'b- 'c)
      'a-b-c)
     
     (test-equal?
      "Symbol append works with exotic arguments"
      (symbol-append 'a- "b-" 'c- 1 '-2 "-3" '- #t "-" #f)
      'a-b-c-1-2-3-true-false)

     ))

  )
