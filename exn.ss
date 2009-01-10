(module exn mzscheme
  
  (require-for-syntax (file "syntax.ss"))

  (require (lib "list.ss" "srfi" "1"))
  
  (require (only (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)) display-exn))
  
  (provide display-exn
           raise-exn
           raise-exn/append
           raise-exn/format
           reraise-exn)

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
  
  ;; syntax raise-exn/append : exception string string ...
  ;;
  ;; DEPRECATED : Use raise-exn instead - it allows you to pass extra arguments
  ;; to the exception constructor.
  (define-syntax (raise-exn/append stx)
    (syntax-case stx ()
      [(_ exception message messages ...)
       (with-syntax ([make-proc (make-syntax-symbol stx 'make- (syntax exception))])
         (syntax
          (raise
           (make-proc
            (string->immutable-string (string-append message messages ...))
            (current-continuation-marks)))))]))
  
  ;; syntax raise-exn/format : exception string any ...
  ;;
  ;; DEPRECATED : Use raise-exn instead - it allows you to pass extra arguments
  ;; to the exception constructor.
  (define-syntax (raise-exn/format stx)
    (syntax-case stx ()
      [(_ exception template params ...)
       (with-syntax ([make-proc (make-syntax-symbol stx 'make- (syntax exception))])
         (syntax
          (raise
           (make-proc
            (string->immutable-string (format template params ...))
            (current-continuation-marks)))))]))
  
  ;; syntax reraise-exn : old-exn new-exn string any ...
  (define-syntax (reraise-exn stx)
    (syntax-case stx ()
      [(_ old-exn new-exn message constructor-args ...)
       (with-syntax ([make-proc (make-syntax-symbol #'new-exn 'make- (syntax new-exn))])
         #'(raise (make-proc (string->immutable-string (string-append message ": " (exn-message old-exn)))
                             (exn-continuation-marks old-exn)
                             constructor-args ...)))]))
  
  )
 
