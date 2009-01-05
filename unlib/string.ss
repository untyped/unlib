#lang scheme/base

(require scheme/contract
         srfi/13/string
         (file "convert.ss"))

; any -> boolean
(define (string+false? item)
  (or (string? item) (not item)))

; (U string bytes any) -> (U string any)
(define (ensure-string str)
  (if (bytes? str)
      (bytes->string/utf-8 str)
      str))

; (listof string) string [#:prefix string] [#:suffix string] -> string
(define (string-delimit items delimiter #:prefix [prefix #f] #:suffix [suffix #f])
  (define delimited (string-join items delimiter))
  (if prefix
      (if suffix
          (string-append prefix delimited suffix)
          (string-append prefix delimited))
      (if suffix
          (string-append delimited suffix)
          delimited)))

; Provide statements ---------------------------

(provide symbol+false->string+false
         string+false->symbol+false
         string+false->number+false
         number+false->string+false)

(provide/contract
 [string+false?   procedure?]
 [ensure-string   procedure?]
 [string-delimit  (->* ((listof string?) string?) (#:prefix (or/c string? false/c) #:suffix (or/c string? false/c)) string?)])
