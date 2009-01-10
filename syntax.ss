(module syntax mzscheme
  
  (require (lib "list.ss" "srfi" "1")
           (lib "stx.ss" "syntax"))
  
  (provide (all-defined))

  (define (syntax-map fn stx)
    (if (stx-null? stx)
        stx
        (cons (fn (stx-car stx))
              (syntax-map fn (stx-cdr stx)))))

  (define (syntax-append-map fn stx)
    (if (stx-null? stx)
        stx
        (concatenate (syntax-map fn stx))))

  (define (symbolic-identifier=? id1 id2)
    (eq? (syntax-object->datum id1)
         (syntax-object->datum id2)))
  
  (define (atom->string atom)
    (cond
      [(string? atom) atom]
      [(symbol? atom) (symbol->string atom)]
      [(number? atom) (number->string atom)]
      [(syntax? atom) (atom->string (syntax-object->datum atom))]
      [else (error "Expected (syntax of) symbol, string or number. "
                   "Received: " atom)]))
  
  (define (make-syntax-symbol stx . args)
    (datum->syntax-object 
     stx
     (string->symbol
      (apply 
       string-append
       (map atom->string args)))))

  )