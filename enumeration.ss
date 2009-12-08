#lang scheme/base

(require (for-syntax scheme/base
                     srfi/26
                     "debug.ss"
                     "enumeration-info.ss"
                     "for.ss"
                     "syntax.ss")
         scheme/provide-syntax
         scheme/string
         "base.ss"
         "enum-internal.ss"
         "exn.ss"
         "match.ss")

; Syntax -----------------------------------------

; (_ id (value-clause ...) keyword-arg ...)
(define-syntax (define-enum complete-stx)
  
  (define id-stx #f)            ; (U syntax #f)
  (define value-id-stxs null)   ; (listof syntax)
  (define value-expr-stxs null) ; (listof syntax)
  (define pretty-stxs null)     ; (listof syntax)
  
  ; syntax syntax -> void
  (define (parse-values stx)
    (syntax-case stx ()
      [() (parse-finish)]
      [([id temp-val str] other ...)
       (identifier? #'id)
       ; Treat value of _ specially:
       (with-syntax ([val (if (eq? (syntax->datum #'temp-val) '_) #''id #'temp-val)])
         (set! value-id-stxs   (cons #'id value-id-stxs))
         (set! value-expr-stxs (cons #'val value-expr-stxs))
         (set! pretty-stxs     (cons #'str pretty-stxs))
         (parse-values #'(other ...)))]
      [([id val] other ...)
       (identifier? #'id)
       (with-syntax ([str (format "~a" (syntax->datum #'id))])
         (parse-values #'([id val str] other ...)))]
      [([id] other ...)
       (identifier? #'id)
       (with-syntax ([str (format "~a" (syntax->datum #'id))])
         (parse-values #'([id 'id str] other ...)))]
      [(id other ...)
       (identifier? #'id)
       (parse-values #'([id] other ...))]
      [(bad-clause other ...)
       (raise-syntax-error #f "bad enum clause" complete-stx #'bad-clause)]))
  
  (define (parse-finish)
    (with-syntax ([id                     id-stx]
                  [private-id             (make-id #f id-stx)]
                  [(value-id ...)         (reverse value-id-stxs)]
                  [(value-private-id ...) (generate-temporaries (reverse value-id-stxs))]
                  [(value ...)            (reverse value-expr-stxs)]
                  [(pretty ...)           (reverse pretty-stxs)])
      #'(begin
          (define value-private-id value) ...
          
          (define private-id
            (make-enum
             'id
             (list value-private-id ...)
             (list (let ([temp pretty])
                     (if (string? temp)
                         temp
                         (raise-type-error 'define-enum "pretty value must be a string" pretty)))
                   ...)))
          
          (define-syntaxes (id)
            (let ([certify (syntax-local-certifier #t)])
              (enum-info-add!
               (make-enum-info
                (certify #'id)
                (certify #'private-id)
                (list (certify #'value-id) ...)
                (list (certify #'value-private-id) ...))))))))
  
  (syntax-case complete-stx ()
    [(_ id (value ...))
     (identifier? #'id)
     (begin (set! id-stx #'id)
            (parse-values #'(value ...)))]))

(define-syntax (enum-list stx)
  (syntax-case stx ()
    [(_ enum-id val-id ...)
     (andmap identifier? (syntax->list #'(enum-id val-id ...)))
     (syntax/loc stx
       (list (enum-id val-id) ...))]))

(define-syntax (enum-complement stx)
  (syntax-case stx ()
    [(_ enum-id val-id ...)
     (andmap identifier? (syntax->list #'(enum-id val-id ...)))
     (let ([val-ids (map syntax->datum (syntax->list #'(val-id ...)))]
           [all-ids (enum-info-value-ids (enum-info-ref #'enum-id))])
       (quasisyntax/loc stx
         (list #,@(for/filter ([id (in-list all-ids)])
                    (and (not (memq (syntax->datum id) val-ids))
                         #`(enum-id #,id))))))]))

(define-syntax (enum-case complete-stx)
  
  ; enum-info syntax -> void
  (define (check-cases info cases-stx)
    (for ([case-stx (in-list (syntax->list cases-stx))])
      (check-case info case-stx)))
  
  ; enum-info syntax -> void
  (define (check-case info case-stx)
    (let ([all-ids (map syntax->datum (enum-info-value-ids info))])
      (unless (andmap identifier? (syntax->list case-stx))
        (raise-syntax-error #f "bad clause syntax: expected (listof value-id)"
                            complete-stx
                            case-stx))
      
      (for ([id-stx (in-list (syntax->list case-stx))])
        (unless (memq (syntax->datum id-stx) all-ids)
          (raise-syntax-error #f "identifier not found in enum" complete-stx id-stx)))))
  
  ; enum-info (listof syntax) -> void
  (define (check-completeness info cases-stx)
    (let* ([case-stxs (syntax->list cases-stx)]
           [all-ids   (map syntax->datum (enum-info-value-ids info))]
           [used-ids  (map syntax->datum (apply append (map syntax->list case-stxs)))])
      (for ([all-id (in-list all-ids)])
        (unless (memq all-id used-ids)
          (raise-syntax-error #f (format "case not covered by clauses: ~a" all-id)
                              complete-stx)))))
  
  ; enum-info (listof syntax) -> void
  (define (find-unused-ids info cases-stx)
    (let* ([case-stxs (syntax->list cases-stx)]
           [all-ids   (map syntax->datum (enum-info-value-ids info))]
           [used-ids  (map syntax->datum (apply append (map syntax->list case-stxs)))])
      (for/fold ([accum null])
                ([all-id (in-list all-ids)])
                (if (memq all-id used-ids)
                    accum
                    (cons all-id accum)))))
  
  (syntax-case* complete-stx (else) symbolic-identifier=?
    
    ; Expression has an 'else' clause:
    [(_ enum-id id [(value-id ...) expr ...] ... [else else-expr ...])
     (identifier? #'enum-id)
     (let* ([info (enum-info-ref #'enum-id)])
       (check-cases info #'((value-id ...) ...))
       (with-syntax ([(unused-id ...) (find-unused-ids info #'((value-id ...) ...))])
         (syntax/loc complete-stx
           (match id
             [(or (eq? (enum-id value-id)) ...) expr ...] ...
             [(or (eq? (enum-id unused-id)) ...) else-expr ...]
             [other else-expr ...]))))]
    
    ; Expression has no 'else' clause:
    [(_ enum-id id [(value-id ...) expr ...] ...)
     (identifier? #'enum-id)
     (let* ([info (enum-info-ref #'enum-id)])
       (check-cases info #'((value-id ...) ...))
       (check-completeness info #'((value-id ...) ...))
       (syntax/loc complete-stx
         (match id
           [(or (eq? (enum-id value-id)) ...) expr ...] ...
           [other (error (format "enum-case ~a: value not in enumeration" 'enum-id)
                         other)])))]))

(define-syntax (enum-lambda stx)
  (syntax-case stx ()
    [(_ enum clause ...)
     (syntax/loc stx
       (lambda (val)
         (enum-case enum val clause ...)))]))

; (_ enum)
; (_ enum id ...)
(define-syntax (in-enum stx)
  (syntax-case stx ()
    [(_ enum)
     (identifier? #'enum)
     #'(in-list (enum-values enum))]
    [(_ enum val ...) 
     (andmap identifier? (syntax->list #'(enum val ...)))
     #'(in-list (enum-list enum val ...))]))

; (_ enum)
; (_ enum id ...)
(define-syntax (in-enum/pretty stx)
  (syntax-case stx ()
    [(_ enum)
     (identifier? #'enum)
     #'(in-list (map (cut enum-prettify enum <>) (enum-values enum)))]
    [(_ enum val ...) 
     (andmap identifier? (syntax->list #'(enum val ...)))
     #'(in-list (map (cut enum-prettify enum <>) (enum-list enum val ...)))]))

; Helpers ----------------------------------------

; (U boolean symbol integer) -> string
(define (enum-value->string val)
  (cond [(boolean? val) (if val "yes" "no")]
        [(symbol? val)  (symbol->string val)]
        [(integer? val) (number->string val)]))

; Provide statements -----------------------------

(provide (all-from-out "enum-internal.ss")
         define-enum
         enum-list
         enum-complement
         enum-case
         enum-lambda
         in-enum
         in-enum/pretty)
