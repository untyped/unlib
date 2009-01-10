(module preprocess mzscheme

  (require (lib "mzpp.ss" "preprocessor"))
  
  (require (planet "namespace.ss" ("schematics" "namespace.plt")))

  ; Procedures -----------------------------------
  
  ;; apply-template : (U string port) (alist-of symbol any) -> string
  (define (apply-template template bindings)
    (let ((op (open-output-string)))
      (parameterize
        ((current-output-port op))
        (let ((ns (make-namespace 'initial)))
          (namespace-attach/require
           ns
           '(lib "mzpp.ss" "preprocessor"))
          (with-namespace
           ns
           (for-each
            (lambda (cell)
              (let ((key (car cell))
                    (val (cdr cell)))
                (eval `(define ,key ,val))))
            bindings)
           (eval `(preprocess ,template)))))
      (get-output-string op)))

  ; Provide statements --------------------------- 
  
  (provide apply-template)
  
  )