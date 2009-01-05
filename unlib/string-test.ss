(module string-test mzscheme
    
  (require (file "string.ss")
           (file "test-base.ss"))
  
  (provide string-tests)
  
  (define string-tests
    (test-suite
     "All tests for string"
     
     (test-equal?
      "string-namecase works on O'"
      (string-namecase "O'GRADY, DON'T LISTEN to o'connor. HE'S crazy.")
      "O'Grady, Don't Listen To O'Connor. He's Crazy.")
     
     (test-equal?
      "string-namecase works on Mac and Mc"
      (string-namecase "Macdonald hates MACDONALD'S ChickeN MCNUGGETS")
      "MacDonald Hates MacDonald's Chicken McNuggets")
     
     (test-equal?
      "string-namecase works on le, van and der"
      (string-namecase "LE COMBER, VAN DER GIEZEN")
      "le Comber, van der Giezen")
     
     (test-equal?
      "ensure-string leaves a string as a string"
      (ensure-string "dave")
      "dave")
     
     (test-equal?
      "ensure-string converts a bytes to a string"
      (ensure-string #"dave")
      "dave")
     
     (test-equal?
      "ensure-string leaves a symbol as is"
      (ensure-string 'dave)
      'dave)
     
     (test-equal?
      "ensure-string leaves #f as is"
      (ensure-string #f)
      #f)
     
     (test-equal?
      "string-delimit with no keyword arguments"
      (string-delimit (list "a" "b" "c")
                      ", ")
      "a, b, c")
     
     (test-equal?
      "string-delimit with #:prefix argument"
      (string-delimit (list "a" "b" "c")
                      ", "
                      #:prefix "[")
      "[a, b, c")
     
     (test-equal?
      "string-delimit with #:suffix argument"
      (string-delimit (list "a" "b" "c")
                      ", "
                      #:suffix "]")
      "a, b, c]")
     
     (test-equal?
      "string-delimit with #:prefix and #:suffix arguments"
      (string-delimit (list "a" "b" "c")
                      ", "
                      #:prefix "["
                      #:suffix "]")
      "[a, b, c]")
     
     ))

  )