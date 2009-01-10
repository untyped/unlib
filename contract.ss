(module contract mzscheme
  
  (require (lib "contract.ss"))
  
  (require (file "base.ss"))
  
  ; Procedures -----------------------------------
  
  (define (symbol-or-false? value)
    (or (symbol? value)
        (eq? value #f)))

  (define (string-or-false? value)
    (or (string? value)
        (eq? value #f)))

  (define (number-or-false? value)
    (or (number? value)
        (eq? value #f)))
  
  (define (integer-or-false? value)
    (or (integer? value)
        (eq? value #f)))

  ; Custom contracts -----------------------------
  
  (define (arity/c num-args)
    (lambda (proc)
      (and (procedure? proc)
           (procedure-arity-includes? proc num-args))))
  
  ; Provide statements --------------------------- 
  
  (provide symbol-or-false?
           string-or-false?
           number-or-false?
           integer-or-false?
           arity/c)
  
  )