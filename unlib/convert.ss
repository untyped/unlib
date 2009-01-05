#lang scheme/base

(require scheme/contract)

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

; Provide statements -----------------------------

(provide/contract
 [number->symbol (-> number? symbol?)]
 [symbol->number (-> symbol? (or/c number? false/c))]
 [number+false->string+false (-> (or/c number? false/c) (or/c string? false/c))]
 [string+false->number+false (-> (or/c string? false/c) (or/c number? false/c))]
 [symbol+false->string+false (-> (or/c symbol? false/c) (or/c string? false/c))]
 [string+false->symbol+false (-> (or/c string? false/c) (or/c symbol? false/c))]
 [number+false->symbol+false (-> (or/c number? false/c) (or/c symbol? false/c))]
 [symbol+false->number+false (-> (or/c symbol? false/c) (or/c number? false/c))])
