#lang scheme/base

(require "test-base.ss")

(require "convert.ss"
         ; These modules are required to test that the identifiers from convert.ss are reprovided cleanly without causing conflicts:
         "number.ss"
         "string.ss"
         "symbol.ss")

; Tests ------------------------------------------

(define/provide-test-suite convert-tests
  
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
    (check-equal? (symbol+false->string+false #f)  #f))
  
  (test-case "natural->hex-string"
    ; Zero:
    (check-equal? (natural->hex-string 0) "0")
    ; Multiple digits:
    (check-equal? (natural->hex-string (+                                        15))    "f")
    (check-equal? (natural->hex-string (+                              (* 14 16) 15))   "ef")
    (check-equal? (natural->hex-string (+                 (* 13 16 16) (* 14 16) 15))  "def")
    (check-equal? (natural->hex-string (+ (* 12 16 16 16) (* 13 16 16) (* 14 16) 15)) "cdef")
    ; Uppercase:
    (check-equal? (natural->hex-string (+                                        15) #:uppercase? #t)    "F")
    (check-equal? (natural->hex-string (+                              (* 14 16) 15) #:uppercase? #t)   "EF")
    (check-equal? (natural->hex-string (+                 (* 13 16 16) (* 14 16) 15) #:uppercase? #t)  "DEF")
    (check-equal? (natural->hex-string (+ (* 12 16 16 16) (* 13 16 16) (* 14 16) 15) #:uppercase? #t) "CDEF")
    ; Padded:
    (check-equal? (natural->hex-string 10000 #:uppercase? #f #:digits 8) "00002710")
    (check-equal? (natural->hex-string 10000 #:uppercase? #f #:digits 4) "2710")
    (check-equal? (natural->hex-string 10000 #:uppercase? #f #:digits 0) "2710")
    ; Prefix:
    (check-equal? (natural->hex-string 10000 #:uppercase? #f #:digits 8 #:prefix? #t) "0x00002710"))
  
  (test-case "hex-string->natural"
    ; Zero:
    (check-equal? (hex-string->natural "0") 0)
    ; Regular:
    (check-equal? (hex-string->natural "C350") 50000)
    (check-equal? (hex-string->natural "c350") 50000)
    (check-equal? (hex-string->natural "0000c350") 50000)
    ; Prefix:
    (check-equal? (hex-string->natural "0x0000c350" #:prefix? #t) 50000)
    ; Error checking:
    (check-exn exn:fail:contract? (cut hex-string->natural ""))
    (check-exn exn:fail:contract? (cut hex-string->natural "abcdefg"))
    (check-exn exn:fail:contract? (cut hex-string->natural "0x0000c350"))
    (check-exn exn:fail:contract? (cut hex-string->natural "0000c350" #:prefix? #t))))
