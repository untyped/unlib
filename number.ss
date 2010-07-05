#lang scheme/base

(require "base.ss")

(require "convert.ss")

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

; number [integer] -> inexact
(define (round-to num [places 0])
  (let ([exp (for/fold ([accum 1])
                       ([num   (in-range 0 (abs places))])
                       (* accum 10))])
    (if (>= places 0)
        (/ (round (* (exact->inexact num) exp)) exp)
        (round (* (round (/ (exact->inexact num) exp)) exp)))))

; Provide statements -----------------------------

(provide symbol+false->number+false
         number+false->symbol+false
         string+false->number+false
         number+false->string+false
         natural->hex-string
         hex-string->natural)

(provide/contract
 [number+false?  procedure?]
 [integer+false? procedure?]
 [natural?       procedure?]
 [natural+false? procedure?]
 [round-to       (->* (number?) (integer?) inexact?)])
