(module number-test mzscheme
  
  (require (file "number.ss")
           (file "test-base.ss"))
  
  (provide number-tests)

  (define number-tests
    (test-suite
     "number.ss"

      (test-case
       "number->symbol converts integers correctly"
       (check-equal? (number->symbol 123) '|123|)
       (check-equal? (number->symbol -123) '|-123|)
       (check-equal? (number->symbol -1.5) '|-1.5|))
       
    ))

  )
