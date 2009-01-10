(module contract-test mzscheme
    
  (require (lib "etc.ss"))
  
  (require (file "contract.ss")
           (file "test-base.ss"))
  
  (provide contract-tests)
  
  (define contract-tests
    (test-suite
     "All tests for contract"
     
     (test-case
      "symbol-or-false? works as expected"
      (check-true  (symbol-or-false? 'sym))
      (check-true  (symbol-or-false? #f))
      (check-false (symbol-or-false? #t))
      (check-false (symbol-or-false? 123))
      (check-false (symbol-or-false? 123.5))
      (check-false (symbol-or-false? "123")))

     (test-case
      "string-or-false? works as expected"
      (check-false (string-or-false? 'sym))
      (check-true  (string-or-false? #f))
      (check-false (string-or-false? #t))
      (check-false (string-or-false? 123))
      (check-false (string-or-false? 123.5))
      (check-true  (string-or-false? "123")))

     (test-case
      "number-or-false? works as expected"
      (check-false (number-or-false? 'sym))
      (check-true  (number-or-false? #f))
      (check-false (number-or-false? #t))
      (check-true  (number-or-false? 123))
      (check-true  (number-or-false? 123.5))
      (check-false (number-or-false? "123")))

     (test-case
      "integer-or-false? works as expected"
      (check-false (integer-or-false? 'sym))
      (check-true  (integer-or-false? #f))
      (check-false (integer-or-false? #t))
      (check-true  (integer-or-false? 123))
      (check-false (integer-or-false? 123.5))
      (check-false (integer-or-false? "123")))
     
     (test-case
      "arity/c works on regular procedures"
      (check-false ((arity/c 1) (lambda () #t)))
      (check-true  ((arity/c 1) (lambda (a) #t)))
      (check-false ((arity/c 1) (lambda (a b) #t))))

     (test-case
      "arity/c works on procedures with rest arguments"
      (check-false ((arity/c 2) (lambda (a b c . d) #t)))
      (check-true  ((arity/c 2) (lambda (a b . c) #t)))
      (check-true  ((arity/c 2) (lambda (a . b) #t)))
      (check-true  ((arity/c 2) (lambda a #t))))

     (test-case
      "arity/c works on opt-lambdas"
      (check-false ((arity/c 2) (opt-lambda (a b c) #t)))
      (check-true  ((arity/c 2) (opt-lambda (a b [c #f]) #t)))
      (check-true  ((arity/c 2) (opt-lambda (a [b #f] [c #f]) #t)))
      (check-true  ((arity/c 2) (opt-lambda ([a #f] [b #f] [c #f]) #t))))

     ))

  )
