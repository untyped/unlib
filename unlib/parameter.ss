(module parameter mzscheme
  
  (require (file "base.ss"))
  
  (provide make-guard
           define-parameter)
  
  ;; make-guard : (any -> boolean) string -> (any -> any)
  ;;
  ;; Makes a procedure that takes a single argument as a parameter and
  ;; checks it against a predicate. If it matches, it returns the value.
  ;; If not, it throws an exception. Useful as a guard procedure for a
  ;; parameter.
  (define (make-guard pred type-message)
    (lambda (val)
      (if (pred val)
          val
          (raise-exn exn:fail:unlib
            (format "Expected ~a, received ~a" type-message val)))))
  
  ;; syntax (define-parameter identifier any (any -> any) identifier)
  (define-syntax (define-parameter stx)
    (syntax-case stx ()
      [(_ parameter-name initial-value guard with-form)
       (syntax
        (begin
          (define parameter-name 
            (make-parameter initial-value guard))
          (define-syntax (with-form stx)
            (syntax-case stx ()
              [(with-form new-value exp (... ...))
               (syntax
                (parameterize ([parameter-name new-value])
                  exp (... ...)))]))))]))

  )