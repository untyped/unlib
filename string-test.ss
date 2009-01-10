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
    
    (test-case "string-ellipsify"
      (check-equal? (string-ellipsify "The quick brown fox jumped over the lazy dog.") "The quick brown f...")
      (check-equal? (string-ellipsify "The quick brown fox.") "The quick brown fox.")
      (check-equal? (string-ellipsify "abc" 1 "...") "abc")
      (check-equal? (string-ellipsify "abcd" 1 "...") "abcd")
      (check-equal? (string-ellipsify "abcd" 4 "...") "abcd")
      (check-equal? (string-ellipsify "abcde" 4 "...") "a...")
      (check-equal? (string-ellipsify "a b c d e " 7 "...") "a b..."))))

; Provide statements -----------------------------

(provide string-tests)
