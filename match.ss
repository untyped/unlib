#lang scheme/base

(require scheme/match
         srfi/26
         "debug.ss")

; Match expanders --------------------------------

; (_ expr pattern ...)
(define-match-expander match:eq?
  (syntax-rules ()
    [(_ expr pattern ...)
     (? (cut eq? <> expr) pattern ...)])
  eq?)

; (_ expr pattern ...)
(define-match-expander match:equal?
  (syntax-rules ()
    [(_ expr pattern ...)
     (? (cut equal? <> expr) pattern ...)])
  equal?)

; (_ proc pattern ...)
(define-match-expander app*
  (syntax-rules ()
    [(_ expr pattern)
     (app expr pattern)]
    [(_ expr pattern ...)
     (app (lambda (val)
            (call-with-values (cut expr val) list))
          (list pattern ...))]))

; Provide statements -----------------------------

(provide (rename-out [match:eq?    eq?]
                     [match:equal? equal?])
         app*)
