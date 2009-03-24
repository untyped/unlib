#lang scheme/base

(require "base.ss")

; Procedures -------------------------------------

; bytes -> natural
(define (crc32 data)
  (bitwise-xor
   (for/fold ([accum #xFFFFFFFF])
             ([byte  (in-bytes data)])
             (for/fold ([accum (bitwise-xor accum byte)])
                       ([num (in-range 0 8)])
                       (bitwise-xor (quotient accum 2)
                                    (* #xEDB88320 (bitwise-and accum 1)))))
   #xFFFFFFFF))

; Provide statements -----------------------------

(provide/contract
 [crc32 (-> bytes? natural-number/c)])