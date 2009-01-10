#lang scheme/base

(require scheme/contract)

; any -> boolean
(define (bytes+false? item)
  (or (bytes? item) (not item)))

; (U bytes string any) -> (U bytes any)
(define (ensure-bytes byt)
  (if (string? byt)
      (string->bytes/utf-8 byt)
      byt))

; Provide statements ---------------------------

(provide/contract
 [bytes+false?   procedure?]
 [ensure-bytes   procedure?])
