#lang scheme/base

(require "base.ss")

(require syntax/boundmap
         "syntax-info.ss"
         (for-template scheme/base))

; Structure types --------------------------------

(define/provide-info-struct enum-info
  (id private-id value-ids values)
  #:transformer
  (lambda (info stx)
    
    (define (raise-enum-syntax-error [sub-stx stx])
      (raise-syntax-error
       #f
       (format "bad enum syntax; possible values are: ~a"
               (map syntax->datum (enum-info-value-ids info)))
       stx
       sub-stx))
    
    (syntax-case stx ()
      [(id)            (identifier? #'id)
                       (raise-enum-syntax-error)]
      [(id val-id)     (and (identifier? #'id)
                            (identifier? #'val-id))
                       (or (for/or ([val-id-stx (in-list (enum-info-value-ids info))]
                                    [val-stx    (in-list (enum-info-values    info))])
                             (and (eq? (syntax->datum #'val-id)
                                       (syntax->datum val-id-stx))
                                  (quasisyntax/loc stx #,val-stx)))
                           (raise-enum-syntax-error))]
      [(id val-id ...) (identifier? #'id)
                       (syntax/loc stx (list (id val-id) ...))]
      [id              (identifier? #'id)
                       (quasisyntax/loc stx #,(enum-info-private-id info))])))

; Variables --------------------------------------

(define info-cache (make-module-identifier-mapping))

; Procedures -------------------------------------

; enum-info -> enum-info
(define (enum-info-add! info)
  (module-identifier-mapping-put! info-cache (enum-info-id info) info)
  info)

; identifier -> boolean
(define (enum-info-set? id)
  (with-handlers ([exn? (lambda _ #f)])
    (module-identifier-mapping-get info-cache id) 
    #t))

; identifier -> enum-info
(define (enum-info-ref id)
  (with-handlers ([exn? (lambda (exn) (raise-syntax-error #f "not an enum identifier" id))])
    (module-identifier-mapping-get info-cache id)))

; Provide statements -----------------------------

(provide/contract
 [enum-info-add! (-> enum-info?  enum-info?)]
 [enum-info-set? (-> identifier? boolean?)]
 [enum-info-ref  (-> identifier? enum-info?)])
