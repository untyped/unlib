#lang scheme/base

(require (for-syntax scheme/base)
         scheme/contract
         scheme/match
         srfi/26)

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

; syntax -> boolean
; syntax -> natural
; syntax -> (listof syntax)
(define-values (dotted-identifier?
                simple-dotted-identifier?
                dotted-identifier-count
                dotted-identifier-split)
  (letrec ([split (lambda (str)
                    (match (regexp-match #px"([^.]*)([.](.*))?" str)
                      [(list all first rest/dot rest)
                       (if (and first rest)
                           (cons first (split rest))
                           (list first))]
                      [#f (error "dang")]))])
    (values (lambda (stx [min-count 2] [max-count #f])
              (and (identifier? stx)
                   (let ([count (length (split (symbol->string (syntax->datum stx))))])
                     (and (or (not min-count) (>= count min-count))
                          (or (not max-count) (<= count max-count))))))
            (lambda (stx [min-count 2] [max-count #f])
              (and (dotted-identifier? stx min-count max-count)
                   (let ([parts (split (symbol->string (syntax->datum stx)))])
                     (andmap (lambda (part)
                               (not (string=? part "")))
                             parts))))
            (lambda (stx)
              (if (identifier? stx)
                  (length (split (symbol->string (syntax->datum stx))))
                  (raise-syntax-error #f "expected identifier" stx)))
            (lambda (stx)
              (if (identifier? stx)
                  (map (lambda (str)
                         (datum->syntax stx (string->symbol str)))
                       (split (symbol->string (syntax->datum stx))))
                  (raise-syntax-error #f "expected dotted identifier" stx))))))

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
 [symbolic-identifier=?     (-> syntax? syntax? boolean?)]
 [make-id                   (->* ((or/c syntax? false/c)) ()
                                 #:rest (listof (or/c string? symbol? number? syntax?))
                                 syntax?)]
 [syntax-location-string    (-> syntax? string?)]
 [dotted-identifier?        (->* (syntax?) ((or/c natural-number/c #f) (or/c natural-number/c #f)) boolean?)]
 [simple-dotted-identifier? (->* (syntax?) ((or/c natural-number/c #f) (or/c natural-number/c #f)) boolean?)]
 [dotted-identifier-count   (-> syntax? natural-number/c)]
 [dotted-identifier-split   (-> syntax? (listof syntax?))])
