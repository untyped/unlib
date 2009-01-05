#lang scheme/base

(require (for-syntax scheme/base)
         scheme/contract
         srfi/26/cut)

; (yield-procedure -> target-procedure) -> target-procedure
;
; where target-procedure and yield-procedure have symmetric contracts:
;
;     target-procedure : a b c -> d e f
;     yield-procedure  : d e f -> a b c
(define (make-yieldable yield->body)
  ; (U continuation #f)
  ; where continuation : (any -> any)
  (define caller #f)
  ; (U continuation #f)
  (define resume #f)
  ; d e f -> a b c
  (define (yield . args)
    (apply values 
           (let/cc k
             (set! resume k)
             (apply caller args))))
  ; d e f -> a b c
  (define (return . args)
    (apply values 
           (let/cc k
             (set! resume #f)
             (apply caller args))))
  ; a b c -> d e f
  (define body
    (yield->body yield))
  ; a b c -> d e f
  (lambda args
    (let/cc k
      (set! caller k)
      (if resume
          (resume args)
          (call-with-values (cut apply body args)
                            return)))))

; syntax (yieldable (id) stmt ...)
(define-syntax (yieldable stx)
  (syntax-case stx ()
    [(_ yield statement ...)
     #'(make-yieldable (lambda (yield) statement ...))]))

; Provide statements -----------------------------

(provide yieldable)

(provide/contract 
 [make-yieldable (-> procedure? procedure?)])
