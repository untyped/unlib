(module number mzscheme
  
  (require (file "base.ss"))
  
  (provide (all-defined))
   
  (define (number->symbol num)
    (string->symbol (number->string num)))

  )