(module all-check-tests mzscheme
  
  (require (file "../test-base.ss")
           (file "check-test.ss"))
  
  (provide all-check-tests)
  
  (define all-check-tests
    (test-suite 
     "all-check-tests"
     
     check-tests
     
     ))
  
  )