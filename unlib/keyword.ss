#lang scheme/base

(require scheme/match
         (only-in srfi/1 zip unzip2)
         "base.ss"
         "exn.ss")

; any ... -> any
;
; A version of keyword-apply that accepts arguments in a more humane order.
(define keyword-apply*
  (match-lambda*
    [(list (? procedure? proc) args ... rest)
     
     ; (listof any) (listof any) (listof any) (listof any) -> any
     ; 
     ; Folds over a list of arguments, collecting keywords, values and other args.
     ; 
     ; Arguments are:
     ;   - args - unprocessed arguments to keyword-apply*;
     ;   - key-accum - accumulator for keyword argument keywords;
     ;   - val-accum - accumulator for keyword argument values;
     ;   - arg-accum - accumulator for non-keyword arguments.
     (define (expand args key-accum val-accum arg-accum)
       (match args
         ; No rest argument:
         [(list)
          (finish key-accum
                  val-accum
                  arg-accum)]
         ; Final argument (i.e. rest argument):
         [(list arg)
          (if (keyword? rest)
              (raise-exn exn:fail:contract
                (format "keyword does not have a value: ~s" rest))
              (expand null
                      key-accum
                      val-accum
                      (cons arg arg-accum)))]
         ; Well formed keyword argument:
         [(list-rest (? keyword? key) val rest)
          (if (keyword? val)
              (raise-exn exn:fail:contract
                (format "keyword does not have a value: ~s" key))
              (expand rest
                      (cons key key-accum)
                      (cons val val-accum)
                      arg-accum))]
         ; Regular argument:
         [(list-rest arg rest)
          (expand rest
                  key-accum
                  val-accum
                  (cons arg arg-accum))]))
     
     ; (listof syntax)  (listof syntax) (listof syntax) -> any
     (define (finish keys vals args)
       (define-values (sorted-keys sorted-vals)
         (unzip2 (sort (zip keys vals)
                       (lambda (kvp1 kvp2)
                         (keyword<? (car kvp1) (car kvp2))))))
       (keyword-apply proc
                      sorted-keys
                      sorted-vals
                      (reverse args)))
     
     (if (or (null? rest) (pair? rest))
         (expand (append args rest) null null null)
         (raise-exn exn:fail:contract
           (format "final argument must be a list: ~s" rest)))]))

; Provide statements -----------------------------

(provide/contract
 [keyword-apply* (->* (procedure? any/c) () #:rest any/c any)])
