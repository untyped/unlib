#lang scheme/base

(require scheme/contract
         srfi/13/string
         (file "base.ss"))

; any -> boolean
(define (symbol+false? item)
  (or (symbol? item) (not item)))

; [(U symbol string)] -> symbol
(define (gensym/interned [base "g"])
  (string->symbol (symbol->string (gensym base))))

; symbol ... -> symbol
(define (symbol-append . args)
  (string->symbol (string-concatenate (map symbol->string args))))

; symbol -> symbol
(define (symbol-upcase sym)
  (string->symbol (string-upcase (symbol->string sym))))

; symbol -> symbol
(define (symbol-downcase sym)
  (string->symbol (string-downcase (symbol->string sym))))

; number -> symbol
(define (number->symbol num)
  (string->symbol (number->string num)))

; symbol -> number
(define (symbol->number sym)
  (string->number (symbol->string sym)))

; Provide statements -----------------------------

(provide/contract
 [symbol+false?   procedure?]
 [gensym/interned (->* () ((or/c symbol? string?)) symbol?)]
 [symbol-append   (->* () () #:rest (listof symbol?) symbol?)]
 [symbol-upcase   (-> symbol? symbol?)]
 [symbol-downcase (-> symbol? symbol?)]
 [number->symbol  (-> number? symbol?)]
 [symbol->number  (-> symbol? (or/c number? false/c))])
