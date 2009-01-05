#lang scheme/base

(require (file "string.ss")
         (file "test-base.ss"))

; Tests ------------------------------------------

(define string-tests
  (test-suite "string.ss"
    
    (test-case "string+false?"
      (check-true (string+false? "dave") "string")
      (check-true (string+false? #f) "false")
      (check-false (string+false? 'dave) "symbol"))
    
    (test-case "ensure-string"
      (check-equal? (ensure-string "dave") "dave" "string")
      (check-equal? (ensure-string #"dave") "dave" "bytes")
      (check-equal? (ensure-string 'dave) 'dave "symbol")
      (check-equal? (ensure-string #f) #f "false"))
    
    (test-case "string-delimit"
      (check-equal? (string-delimit (list "a" "b" "c") ", ")                            "a, b, c"  "no prefix, no suffix")
      (check-equal? (string-delimit (list "a" "b" "c") ", " #:prefix "[")              "[a, b, c"  "prefix, no suffix")
      (check-equal? (string-delimit (list "a" "b" "c") ", " #:suffix "]")               "a, b, c]" "no prefix, suffix")
      (check-equal? (string-delimit (list "a" "b" "c") ", " #:prefix "[" #:suffix "]") "[a, b, c]" "prefix, suffix"))
    
    ))

; Provide statements -----------------------------

(provide string-tests)
