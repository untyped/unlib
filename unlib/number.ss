#lang scheme/base

(require scheme/contract
         (file "base.ss")
         (file "convert.ss"))

; Procedures -------------------------------------

; any -> boolean
(define (number+false? item)
  (or (number? item) (not item)))

; any -> boolean
(define (integer+false? item)
  (or (integer? item) (not item)))

; any -> boolean
(define (natural? num)
  (and (integer? num)
       (>= num 0)))

; any -> boolean
(define (natural+false? item)
  (or (natural? item) (not item)))

; Provide statements -----------------------------

(provide symbol+false->number+false
         number+false->symbol+false
         string+false->number+false
         number+false->string+false)

(provide/contract
 [number+false?  procedure?]
 [integer+false? procedure?]
 [natural?       procedure?]
 [natural+false? procedure?])
