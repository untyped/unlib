#lang scheme/base

(require (file "bytes.ss")
         (file "test-base.ss"))

; Tests ------------------------------------------

(define bytes-tests
  (test-suite "bytes.ss"
    
    (test-case "bytes+false?"
      (check-true (bytes+false? #"dave") "bytes")
      (check-true (bytes+false? #f) "false")
      (check-false (bytes+false? "dave") "string"))
    
    (test-case "ensure-bytes"
      (check-equal? (ensure-bytes "dave") #"dave" "string")
      (check-equal? (ensure-bytes #"dave") #"dave" "bytes")
      (check-equal? (ensure-bytes 'dave) 'dave "symbol")
      (check-equal? (ensure-bytes #f) #f "false"))))

; Provide statements -----------------------------

(provide bytes-tests)
