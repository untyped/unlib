#lang scheme/base

(require "base.ss")

(require (for-syntax scheme/base
                     "syntax.ss")
         scheme/contract
         scheme/struct-info)

(define-syntax (define/provide-info-struct stx)
  (syntax-case stx ()
    [(_ info (field ...))
     #'(define/provide-info-struct info (field ...) #:struct? #f #:transformer #f)]
    [(_ info (field ...) #:transformer proc)
     #'(define/provide-info-struct info (field ...) #:struct? #f #:transformer proc)]
    [(_ info (field ...) #:struct? struct?)
     #'(define/provide-info-struct info (field ...) #:struct? struct? #:transformer #f)]
    [(_ info (field ...) #:struct? struct? #:transformer proc)
     (with-syntax ([struct:info    (make-id #'info 'struct: #'info)]
                   [make-info      (make-id #'info 'make- #'info)]
                   [info?          (make-id #'info #'info '?)]
                   [info-ref       (make-id #'info #'info '-ref)]
                   [info-set!      (make-id #'info #'info '-set!)]
                   [num-fields     (length (syntax->list #'(field ...)))]
                   [(accessor ...) (for/list ([field-stx (in-list (syntax->list #'(field ...)))])
                                          (make-id #'info #'info '- field-stx))])
       #'(begin (define-values (make-info info? accessor ...)
                  (letrec-values ([(struct:info make-info info? info-ref info-set!)
                                   (make-struct-type 'info (and struct? struct:struct-info) num-fields 0 #f null #f proc)]
                                  [(accessor ...)
                                   (apply values (for/list ([index (in-naturals)]
                                                            [name  (in-list '(field ...))])
                                                   (make-struct-field-accessor info-ref index name)))])
                    (values make-info info? accessor ...)))
                (provide make-info info? accessor ...)))]))

; Provide statements -----------------------------

(provide define/provide-info-struct)
