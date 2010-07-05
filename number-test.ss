#lang scheme/base

(require "number.ss"
         "test-base.ss")

; Tests ------------------------------------------

(define/provide-test-suite number-tests
  
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
  
  (test-case "round-to"
    ; Positives:
    (check-equal? (round-to 123.456  0)   123.0)
    (check-equal? (round-to 123.456  1)   123.5)
    (check-equal? (round-to 123.456  2)   123.46)
    (check-equal? (round-to 123.456  3)   123.456)
    (check-equal? (round-to 123.456  4)   123.456)
    ; Negatives:
    (check-equal? (round-to 654.321  0)   654.0)
    (check-equal? (round-to 654.321 -1)   650.0)
    (check-equal? (round-to 654.321 -2)   700.0)
    (check-equal? (round-to 654.321 -3)  1000.0)
    (check-equal? (round-to 654.321 -4) 00000.0))
  
  (test-case "reprovided identifiers from convert.ss"
    (for-each (lambda (proc)
                (check-pred procedure? proc))
              (list number+false->string+false
                    string+false->number+false
                    number+false->symbol+false
                    symbol+false->number+false
                    natural->hex-string
                    hex-string->natural))))
