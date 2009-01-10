#lang scheme/base

(require "convert.ss"
         "test-base.ss")

; Tests ------------------------------------------

(define convert-tests
  (test-suite "convert.ss"
        
    (test-equal? "number->symbol"
      (number->symbol 123)
      '|123|)
    
    (test-case "symbol->number"
      (check-equal? (symbol->number '|123|) 123 "numeric equivalent")
      (check-equal? (symbol->number 'abc) #f "no numeric equivalent"))
    
    (test-case "number+false->symbol+false"
      (check-equal? (number+false->symbol+false 123) '|123|)
      (check-equal? (number+false->symbol+false #f)  #f))
    
    (test-case "number+false->string+false"
      (check-equal? (number+false->string+false 123) "123")
      (check-equal? (number+false->string+false #f)  #f))
    
    (test-case "string+false->symbol+false"
      (check-equal? (string+false->symbol+false "123") '|123|)
      (check-equal? (string+false->symbol+false #f)  #f))
    
    (test-case "string+false->number+false"
      (check-equal? (string+false->number+false "123") 123)
      (check-equal? (string+false->number+false #f)  #f))
    
    (test-case "symbol+false->number+false"
      (check-equal? (symbol+false->number+false '|123|) 123)
      (check-equal? (symbol+false->number+false #f)  #f))
    
    (test-case "symbol+false->string+false"
      (check-equal? (symbol+false->string+false '|123|) "123")
      (check-equal? (symbol+false->string+false #f)  #f))))

; Provide statements -----------------------------

(provide convert-tests)
