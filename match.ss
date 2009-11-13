#lang scheme/base

(require "base.ss")

(require (for-syntax scheme/base
                     (cce-scheme-in syntax))
         scheme/match
         srfi/26
         "debug.ss")

; Match expanders --------------------------------

; (_ expr pattern ...)
(define-match-expander match:eq?
  (lambda (stx)
    (syntax-case stx ()
      [(_ expr pattern ...)
       #'(? (cut eq? <> expr) pattern ...)]))
  (redirect-transformer #'eq?))

; (_ expr pattern ...)
(define-match-expander match:equal?
  (lambda (stx)
    (syntax-case stx ()
      [(_ expr pattern ...)
       #'(? (cut equal? <> expr) pattern ...)]))
  (redirect-transformer #'equal?))

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
