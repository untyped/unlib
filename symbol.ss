#lang scheme/base

(require srfi/13
         "base.ss"
         "convert.ss"
         "number.ss")

; any -> boolean
(define (symbol+false? item)
  (or (symbol? item) (not item)))

; [(U symbol string)] -> symbol
(define (gensym/interned [base "g"])
  (string->symbol (symbol->string (gensym base))))

; symbol ... -> symbol
(define (symbol-append . args)
  (string->symbol (string-concatenate (map symbol->string args))))

; symbol -> natural
(define (symbol-length sym)
  (string-length (symbol->string sym)))

; symbol -> symbol
(define (symbol-upcase sym)
  (string->symbol (string-upcase (symbol->string sym))))

; symbol -> symbol
(define (symbol-downcase sym)
  (string->symbol (string-downcase (symbol->string sym))))

; Provide statements -----------------------------

(provide symbol->number
         number->symbol
         symbol+false->string+false
         string+false->symbol+false
         symbol+false->number+false
         number+false->symbol+false)

(provide/contract
 [symbol+false?   procedure?]
 [gensym/interned (->* () ((or/c symbol? string?)) symbol?)]
 [symbol-append   (->* () () #:rest (listof symbol?) symbol?)]
 [symbol-length   (-> symbol? natural?)]
 [symbol-upcase   (-> symbol? symbol?)]
 [symbol-downcase (-> symbol? symbol?)])
