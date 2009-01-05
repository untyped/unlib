#lang mzscheme

(require (file "number.ss")
         (file "test-base.ss"))

; Tests ------------------------------------------

(define number-tests
  (test-suite "number.ss"
    
    (test-case "number+false?"
      (check-false (number+false? "dave") "string")
      (check-true (number+false? #f) "false")
      (check-true (number+false? 123.456) "real")
      (check-true (number+false? 123) "integer"))
    
    (test-case "integer+false?"
      (check-false (integer+false? "dave") "string")
      (check-true (integer+false? #f) "false")
      (check-false (integer+false? 123.456) "real")
      (check-true (integer+false? 123) "integer"))
    
    (test-case "natural?"
      (check-false (natural? #f) "false")
      (check-false (natural? -1) "negative")
      (check-true (natural? 0) "zero")
      (check-true (natural? 1) "positive")
      (check-false (natural? 1.5) "inexact")
      (check-false (natural? (/ 1 2)) "exact"))
    
    (test-case "natural+false?"
      (check-true (natural+false? #f) "false")
      (check-false (natural+false? -1) "negative")
      (check-true (natural+false? 0) "zero")
      (check-true (natural+false? 1) "positive")
      (check-false (natural+false? 1.5) "inexact")
      (check-false (natural+false? (/ 1 2)) "exact"))
    
    ))

; Provide statements -----------------------------

(provide number-tests)
