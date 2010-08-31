#lang scheme/base

(require (for-syntax scheme/base
                     srfi/26
                     "debug.ss"
                     "enumeration-info.ss"
                     "syntax.ss")
         scheme/provide-syntax
         scheme/string
         "base.ss"
         "exn.ss"
         "match.ss")

; Structure types --------------------------------

; (struct symbol (listof any) (listof string) (any any -> boolean))
(define-struct enum (name values pretty-values equality-test) #:transparent)

; Accessors and mutators -------------------------

; enum -> string
(define (enum->string enum [separator ", "])
  (string-join (map enum-value->string (enum-values enum)) separator))

; enum -> string
(define (enum->pretty-string enum [separator ", "])
  (string-join (enum-pretty-values enum) separator))

; enum any -> boolean
(define (enum-value? enum value)
  (and (memf (cut enum-same? enum <> value)
             (enum-values enum))
       #t))

; enum -> contract
(define (enum-value/c enum)
  (flat-named-contract
   `(enum-value/c ,(enum-name enum))
   (cute enum-value? enum <>)))

; enum any -> boolean
(define (enum-value+false? enum value)
  (or (not value) (enum-value? enum value)))

; enum any [(U any (-> any))] -> (U string #f)
(define (enum-prettify
         enum
         value
         [default (cut raise-type-error
                       'enum-prettify
                       (format "(U ~a)" (enum->string enum " "))
                       value)])
  (or (for/or ([val (enum-values enum)] [str (enum-pretty-values enum)])
        (and (enum-same? enum val value) str))
      (if (procedure? default)
          (default)
          default)))

; enum any any -> boolean
(define (enum-same? enum a b)
  ((enum-equality-test enum) a b))

; Helpers ----------------------------------------

; (U boolean symbol integer) -> string
(define (enum-value->string val)
  (cond [(boolean? val) (if val "yes" "no")]
        [(symbol? val)  (symbol->string val)]
        [(integer? val) (number->string val)]
        [(string? val)  val]
        [else           (format "~a" val)]))

; Provide statements -----------------------------

(provide/contract
 [struct enum ([name          symbol?]
               [values        list?]
               [pretty-values (listof string?)]
               [equality-test (-> any/c any/c boolean?)])]
 [enum->string        (->* (enum?) (string?) string?)]
 [enum->pretty-string (->* (enum?) (string?) string?)]
 [enum-value?         (-> enum? any/c boolean?)]
 [enum-value/c        (-> enum? flat-contract?)]
 [enum-value+false?   (-> enum? any/c boolean?)]
 [enum-prettify       (->* (enum? any/c) ((or/c string? (-> string?))) string?)]
 [enum-same?          (-> enum? any/c any/c boolean?)])
