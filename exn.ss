(module exn mzscheme
  
  (require-for-syntax (file "syntax.ss"))

  (require (only (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)) display-exn))
  
  (require (lib "contract.ss")
           (all-except (lib "list.ss" "srfi" "1") any))
  
  ;; syntax raise-exn : exception string
  ;;
  ;; TODO : Check at expansion-time whether exception actually extends exn.
  (define-syntax (raise-exn stx)
    (syntax-case stx ()
      [(_ exception message extra-args ...)
       (with-syntax ([make-proc (make-syntax-symbol stx 'make- (syntax exception))])
         #'(raise (apply make-proc
                         (list (string->immutable-string message)
                               (current-continuation-marks)
                               extra-args ...))))]))
  
  ;; syntax reraise-exn : old-exn new-exn string any ...
  (define-syntax (reraise-exn stx)
    (syntax-case stx ()
      [(_ old-exn new-exn message constructor-args ...)
       (with-syntax ([make-proc (make-syntax-symbol #'new-exn 'make- (syntax new-exn))])
         #'(raise (make-proc (string->immutable-string (string-append message ": " (exn-message old-exn)))
                             (exn-continuation-marks old-exn)
                             constructor-args ...)))]))

  ; Provide statements --------------------------- 
  
  (provide display-exn
           raise-exn
           reraise-exn)

  )
 
