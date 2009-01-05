#lang scheme/base

(require srfi/13
         (file "base.ss"))

; (struct symbol integer)
(define-struct timer (name [value #:mutable]) 
  #:transparent
  #:property prop:custom-write
  (lambda (timer out write?)
    (define name  (timer-name timer))
    (define value (floor (timer-value timer)))
    (define min   (inexact->exact (floor (/ value (* 60 1000)))))
    (define sec   (inexact->exact (floor (/ (remainder value (* 60 1000)) 1000))))
    (define milli (string-pad (number->string (inexact->exact (remainder value 1000))) 3 #\0))
    (fprintf out "#<timer:~a ~am ~a.~as>" name min sec milli)))

; Provide statements -----------------------------

; contract
(define value/c (and/c number? (>=/c 0)))

(provide value/c)

(provide/contract
 [struct timer ([name symbol?] [value value/c])])
