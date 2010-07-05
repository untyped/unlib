#lang scheme/base

(require "base.ss")

(require (only-in srfi/13 string-pad string-drop string-take))

; number -> symbol
(define (number->symbol num)
  (string->symbol (number->string num)))

; symbol -> number
(define (symbol->number sym)
  (string->number (symbol->string sym)))

; (U number #f) -> (U string #f)
(define (number+false->string+false num)
  (and num (number->string num)))

; (U string #f) -> (U number #f)
(define (string+false->number+false str)
  (and str (string->number str)))

; (U symbol #f) -> (U string #f)
(define (symbol+false->string+false num)
  (and num (symbol->string num)))

; (U string #f) -> (U symbol #f)
(define (string+false->symbol+false sym)
  (and sym (string->symbol sym)))

; (U number #f) -> (U symbol #f)
(define (number+false->symbol+false num)
  (and num (number->symbol num)))

; (U symbol #f) -> (U number #f)
(define (symbol+false->number+false sym)
  (and sym (symbol->number sym)))

; Hexadecimal ------------------------------------

; natural [#:uppercase? boolean] [#:digits (U natural #f)] [#:prefix? boolean] -> string
(define (natural->hex-string num #:uppercase? [uppercase? #f] #:digits [digits 1] #:prefix? [prefix? #f])
  
  (define (make-digit num)
    (case num
      [( 0) #\0] [( 1) #\1] [( 2) #\2] [( 3) #\3]
      [( 4) #\4] [( 5) #\5] [( 6) #\6] [( 7) #\7]
      [( 8) #\8] [( 9) #\9] [(10) #\a] [(11) #\b]
      [(12) #\c] [(13) #\d] [(14) #\e] [(15) #\f]))
  
  (define (make-digits num)
    (if (zero? num)
        null
        (cons (make-digit  (remainder num 16))
              (make-digits (quotient  num 16)))))
  
  (let* ([ans0 (apply string (reverse (make-digits num)))]
         ; Pad with extra zeroes:
         [ans1 (if (and digits (< (string-length ans0) digits))
                   (string-pad ans0 digits #\0)
                   ans0)]
         ; Do case conversion:
         [ans2 (if uppercase?
                   (string-upcase ans1)
                   ans1)]
         ; Add "0x" prefix:
         [ans3 (if prefix?
                   (string-append "0x" ans2)
                   ans2)])
    ans3))

; string [#:prefix? boolean] -> natural
(define (hex-string->natural str #:prefix? [prefix? #f])
  
  (define (type-error)
    (if prefix?
        (raise-type-error 'hex-string->natural "hex string with \"0x\" prefix" str)
        (raise-type-error 'hex-string->natural "hex string" str)))
  
  (define (parse-digit chr)
    (case chr
      [(#\0)  0] [(#\1)  1] [(#\2)  2] [(#\3)  3]
      [(#\4)  4] [(#\5)  5] [(#\6)  6] [(#\7)  7]
      [(#\8)  8] [(#\9)  9] [(#\a) 10] [(#\b) 11]
      [(#\c) 12] [(#\d) 13] [(#\e) 14] [(#\f) 15]
      [else (raise-type-error 'hex-string->natural "hex string" str)]))
  
  (let ([str (if prefix? 
                 (if (regexp-match #rx"^0x" str)
                     (string-drop str 2)
                     (type-error))
                 (if (zero? (string-length str))
                     (type-error)
                     str))])
    (let loop ([digits (reverse (map parse-digit (string->list (string-downcase str))))] [exponent 1] [accum 0])
      (if (null? digits)
          accum
          (loop (cdr digits)
                (* exponent 16)
                (+ accum (* (car digits) exponent)))))))

; Provide statements -----------------------------

(provide/contract
 [number->symbol             (-> number? symbol?)]
 [symbol->number             (-> symbol? (or/c number? #f))]
 [number+false->string+false (-> (or/c number? #f) (or/c string? #f))]
 [string+false->number+false (-> (or/c string? #f) (or/c number? #f))]
 [symbol+false->string+false (-> (or/c symbol? #f) (or/c string? #f))]
 [string+false->symbol+false (-> (or/c string? #f) (or/c symbol? #f))]
 [number+false->symbol+false (-> (or/c number? #f) (or/c symbol? #f))]
 [symbol+false->number+false (-> (or/c symbol? #f) (or/c number? #f))]
 [natural->hex-string        (->* (natural-number/c) (#:uppercase? boolean? #:digits (or/c natural-number/c #f) #:prefix? boolean?) string?)]
 [hex-string->natural        (->* (string?) (#:prefix? boolean?) natural-number/c)])
