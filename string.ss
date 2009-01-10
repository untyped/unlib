#lang scheme/base

(require scheme/contract
         srfi/13/string)

; any -> boolean
(define (string+false? item)
  (or (string? item) (not item)))

; (U string bytes any) -> (U string any)
(define (ensure-string str)
  (cond [(string? str) str]
        [(bytes? str)  (bytes->string/utf-8 str)]
        [else          str]))

; (listof string) string [#:prefix string] [#:suffix string] -> string
(define (string-delimit items delimiter #:prefix [prefix #f] #:suffix [suffix #f])
    (let ([delimited (string-join items delimiter)])
      (if prefix
          (if suffix
              (string-append prefix delimited suffix)
              (string-append prefix delimited))
          (if suffix
              (string-append delimited suffix)
              delimited))))

; Provide statements ---------------------------

(provide/contract
 [string+false?   procedure?]
 [ensure-string   procedure?]
 [string-delimit  (->* ((listof string?) string?) (#:prefix (or/c string? false/c) #:suffix (or/c string? false/c)) string?)])
