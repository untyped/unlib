#lang scheme/base

(require "base.ss"
         "pipeline.ss")

; Test data ------------------------------------

; Target function

(define (target arg)
  (list 'target arg))

; Basic pipeline

(define (stage-a continue arg) 
  (cons 'a (cons arg (continue (+ arg 1)))))

(define (stage-b continue arg) 
  (cons 'b (cons arg (continue (+ arg 2)))))

(define (stage-c continue arg) 
  (cons 'c (cons arg (continue (+ arg 3)))))

; Replacement stages

(define (stage-b2 continue arg) 
  (cons 'b (cons arg (continue (+ arg 4)))))

(define (stage-b3 continue arg)
  (cons 'b (cons arg null)))

; Provide statements --------------------------- 

(provide (all-defined-out))
