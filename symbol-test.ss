#lang scheme/base

(require "symbol.ss"
         "test-base.ss")

; Tests ------------------------------------------

(define/provide-test-suite symbol-tests
  
  (test-case "symbol+false?"
    (check-false (symbol+false? "dave") "string")
    (check-true (symbol+false? #f) "false")
    (check-true (symbol+false? 'dave) "symbol"))
  
  (test-case "gensym/interned"
    (let ([sym1 (gensym)]
          [sym2 (gensym/interned)]
          [sym3 (gensym/interned 'x)])
      (check-false (equal? sym1 (string->symbol (symbol->string sym1))) "uninterned")
      (check-true (equal? sym2 (string->symbol (symbol->string sym2))) "interned")
      (check-true (and (regexp-match #rx"g[0-9]+" (symbol->string sym2)) #t) "format with default base")
      (check-true (and (regexp-match #rx"x[0-9]+" (symbol->string sym3)) #t) "format with alternative base")))
  
  (test-equal? "symbol-append"
    (symbol-append 'a- 'b- 'c)
    'a-b-c)
  
  (test-case "symbol-length"
    (check-equal? (symbol-length 'abc) 3)
    (check-equal? (symbol-length '||) 0))
  
  (test-equal? "symbol-upcase"
    (symbol-upcase 'aBcDe12345)
    'ABCDE12345)
  
  (test-equal? "symbol-downcase"
    (symbol-downcase 'aBcDe12345)
    'abcde12345))
