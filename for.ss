#lang scheme/base

(require "base.ss")

(require (for-syntax scheme/base
                     (cce-scheme-in syntax)))


(define-syntax (for/fold/reverse stx)
  (syntax-case stx ()
    [(_ (accum ...) (seq ...) expr ...)
     (with-syntax ([(temp ...) (generate-temporaries #'(accum ...))])
       (syntax/loc stx
         (let-values ([(temp ...) (for/fold (accum ...) (seq ...) expr ...)])
           (values (reverse temp) ...))))]))

(define-syntax (for/fold1 stx)
  (syntax-case stx ()
    [(_ (accum ...) (seq ...) expr ...)
     (with-syntax* ([(temp ...) (generate-temporaries #'(accum ...))]
                    [ans        (car (syntax->list #'(temp ...)))])
       (syntax/loc stx
         (let-values ([(temp ...) (for/fold (accum ...) (seq ...) expr ...)])
           ans)))]))

(define-syntax (for/fold1/reverse stx)
  (syntax-case stx ()
    [(_ (accum ...) (seq ...) expr ...)
     (with-syntax* ([(temp ...) (generate-temporaries #'(accum ...))]
                    [ans        (car (syntax->list #'(temp ...)))])
       (syntax/loc stx
         (let-values ([(temp ...) (for/fold (accum ...) (seq ...) expr ...)])
           (reverse ans))))]))

(define-syntax (for/filter stx)
  (syntax-case stx ()
    [(_ (seq ...) expr ...)
     (with-syntax ([(accum ...) (generate-temporaries #'(seq ...))]
                   [(ans   ...) (generate-temporaries #'(seq ...))])
       (syntax/loc stx
         (for/fold/reverse
          ([accum null] ...)
          (seq ...)
          (let-values ([(ans ...) (begin expr ...)])
            (if ans (cons ans accum) accum)
            ...))))]))

(define-syntax (for/filter1 stx)
  (syntax-case stx ()
    [(_ (seq ...) expr ...)
     (syntax/loc stx
       (for/fold1/reverse
        ([accum null])
        (seq ...)
        (let ([ans (begin expr ...)])
          (if ans (cons ans accum) accum))))]))

(define-syntax (for/append stx)
  (syntax-case stx ()
    [(_ (seq ...) expr ...)
     (syntax/loc stx
       (apply append (for/list (seq ...) expr ...)))]))

; Provide statements -----------------------------

(provide for/fold/reverse
         for/fold1
         for/fold1/reverse
         for/filter
         for/filter1
         for/append)
