#lang scheme/base

(require (for-syntax scheme/base)
         (file "base.ss"))

; Variables --------------------------------------

; continuation-prompt-tag
(define yield-prompt
  (make-continuation-prompt-tag 'yield))

; New implementation -----------------------------

; (yield-procedure -> target-procedure) -> target-procedure
(define (make-yieldable/composable-continuations yield->body)
  ; (U continuation #f)
  ; where continuation : (any -> any)
  (define caller #f)
  ; (U continuation #f)
  (define resume #f)
  ; d e f -> a b c
  (define (yield . args)
    (apply values 
           (call/cc
            (lambda (k)
              (set! resume k)
              (apply caller args))
            yield-prompt)))
  ; d e f -> a b c
  (define (return . args)
    (apply values 
           (call/cc 
            (lambda (k)
              (set! resume #f)
              (apply caller args))
            yield-prompt)))
  ; a b c -> d e f
  (define body
    (yield->body yield))
  ; a b c -> d e f
  (lambda args
    (call-with-continuation-prompt
     (lambda ()
       (call/cc 
        (lambda (k)
          (set! caller k)
          (if resume
              (resume args)
              (call-with-values (cut apply body args)
                                return)))
        yield-prompt))
     yield-prompt)))

; Old implementation -----------------------------

; (yield-procedure -> target-procedure) -> target-procedure
(define (make-yieldable/full-continuations yield->body)
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

; Main interface ---------------------------------

; (yield-procedure -> target-procedure) -> target-procedure
;
; where target-procedure and yield-procedure have symmetric contracts:
;
;     target-procedure : a b c -> d e f
;     yield-procedure  : d e f -> a b c
(define make-yieldable
  make-yieldable/composable-continuations)

; syntax (yieldable (id) stmt ...)
(define-syntax (yieldable stx)
  (syntax-case stx ()
    [(_ yield statement ...)
     #'(make-yieldable (lambda (yield) statement ...))]))

; Provide statements -----------------------------

(provide yieldable)

(provide/contract 
 [make-yieldable                          (-> procedure? procedure?)]
 [make-yieldable/composable-continuations (-> procedure? procedure?)]
 [make-yieldable/full-continuations       (-> procedure? procedure?)])
