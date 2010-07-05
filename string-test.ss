#lang scheme/base

(require "string.ss"
         "test-base.ss")

; Tests ------------------------------------------

(define/provide-test-suite string-tests
  
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
    (check-equal? (string-ellipsify "a b c d e " 7 "...") "a b..."))
  
  (test-case "string-titlecase*"
    (check-equal? (string-titlecase* "lowercase to titlecase")        "Lowercase To Titlecase")
    (check-equal? (string-titlecase* "InterCapsed stays interCapsed") "InterCapsed Stays InterCapsed")
    (check-equal? (string-titlecase* "ALLCAPS stays ALLCAPSED")       "ALLCAPS Stays ALLCAPSED"))
  
  (test-case "string-sentencecase"
    (check-equal? (string-sentencecase "lowercase to capitalised")      "Lowercase to capitalised")
    (check-equal? (string-sentencecase "Capitalised stays capitalised") "Capitalised stays capitalised")
    (check-equal? (string-sentencecase "ALLCAPS stays ALLCAPSED")       "ALLCAPS stays ALLCAPSED"))
  
  (test-case "reprovided identifiers from convert.ss"
    (for-each (lambda (proc)
                (check-pred procedure? proc))
              (list number+false->string+false
                    string+false->number+false
                    string+false->symbol+false
                    symbol+false->string+false
                    natural->hex-string
                    hex-string->natural))))
