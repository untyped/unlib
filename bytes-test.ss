#lang scheme/base

(require "bytes.ss"
         "test-base.ss")

; Tests ------------------------------------------

(define/provide-test-suite bytes-tests
  
  (test-case "bytes+false?"
    (check-true (bytes+false? #"dave") "bytes")
    (check-true (bytes+false? #f) "false")
    (check-false (bytes+false? "dave") "string"))
  
  (test-case "ensure-bytes"
    (check-equal? (ensure-bytes "dave") #"dave" "string")
    (check-equal? (ensure-bytes #"dave") #"dave" "bytes")
    (check-equal? (ensure-bytes 'dave) 'dave "symbol")
    (check-equal? (ensure-bytes #f) #f "false")))
