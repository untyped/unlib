#lang mzscheme

(require scheme/contract
         (file "base.ss"))

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

(provide/contract
 [number+false?  procedure?]
 [integer+false? procedure?]
 [natural?       procedure?]
 [natural+false? procedure?])
