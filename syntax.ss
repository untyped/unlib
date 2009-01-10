#lang scheme/base

(require (for-syntax scheme/base)
         scheme/contract
         scheme/match)

; syntax syntax -> boolean
(define (symbolic-identifier=? id1 id2)
  (eq? (syntax->datum id1)
       (syntax->datum id2)))

; syntax (U string symbol number syntax) ... -> syntax
(define (make-id stx . args)
  (datum->syntax stx (string->symbol (apply string-append (map atom->string args)))))

; (_ (define ...) ...)
(define-syntax (begin-for-syntax/any-order stx)
  (define (expand-definition-name stx)
    (syntax-case stx (define)
      [(define (name arg ...) expr ...) #'name]
      [(define name expr)               #'name]))
  (define (expand-definition stx)
    (syntax-case stx (define)
      [(define (name arg ...) expr ...) #'(name (lambda (arg ...) expr ...))]
      [(define name expr)               #'(name expr)]))
  (syntax-case stx ()
    [(_ definition ...)
     (let* ([definitions    (syntax->list #'(definition ...))]
            [names          (map expand-definition-name definitions)]
            [letrec-clauses (map expand-definition definitions)])
       #`(define-values-for-syntax #,names
           (letrec #,letrec-clauses
             (values #,@names))))]))

; syntax -> string
(define (syntax-location-string stx)
  ; string
  (define source
    (match (syntax-source stx)
      [(? path? src)
       (let-values ([(base name must-be-dir?) (split-path src)])
         name)]
      [(? string? src)
       (match (regexp-match #rx"[^\\/\\\\]+$" src)
         [(list filename) filename]
         [other "unknown.ss"])]
      [other "unknown.ss"]))
  ; (U integer "")
  (define line
    (or (syntax-line stx) ""))
  ; (U integer "")
  (define column
    (or (and (syntax-line stx) (syntax-column stx))
        (syntax-position stx)
        ""))
  ; string
  (format "~a:~a:~a" source line column))

; Helpers ----------------------------------------

; (U string symbol number syntax) -> string
(define (atom->string atom)
  (cond [(string? atom) atom]
        [(symbol? atom) (symbol->string atom)]
        [(number? atom) (number->string atom)]
        [(syntax? atom) (atom->string (syntax->datum atom))]
        [else (error "Expected (syntax of) (U symbol string number), received: " atom)]))

; Provide statements -----------------------------

(provide begin-for-syntax/any-order)

(provide/contract
 [symbolic-identifier=?  (-> syntax? syntax? boolean?)]
 [make-id                (->* ((or/c syntax? false/c)) ()
                              #:rest (listof (or/c string? symbol? number? syntax?))
                              syntax?)]
 [syntax-location-string (-> syntax? string?)])
