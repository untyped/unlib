(module debug mzscheme
  
  (require (lib "etc.ss")
           (lib "pretty.ss")
           (file "base.ss")
           (file "log.ss")
           (file "parameter.ss"))

  (require-for-syntax (file "syntax.ss"))
  
  (provide (all-defined))
 
  (define debug-log (make-log 'D))

  ;; parameter debug-enabled? : boolean
  (define-parameter debug-enabled?
    #t
    (lambda (val)
      (if (boolean? val)
          val
          (raise-exn exn:fail:unlib
           (format "Expected boolean, received ~a." val))))
    with-debug-enabled?)
  
  ;; debug : string 'a -> 'a
  ;;
  ;; Prints the value of the specified expression and then returns it
  ;; transparently.
  (define (debug message value)
    (if (debug-enabled?)
        (log-generic debug-log (list message value))
        (void))
    value)
  
  (define-syntax (define-debug stx)
    (syntax-case stx ()
      [(_ var val)
       #`(define var (debug (symbol->string 'var) val))]))
  
  (define-syntax (let-debug stx)
    (syntax-case stx ()
      [(_ ([var val] ...) exp ...)
       #'(let ([var (debug (symbol->string 'var) val)] ...)
           exp ...)]))
       
  (define-syntax (let*-debug stx)
    (syntax-case stx ()
      [(_ ([var val] ...) exp ...)
       #'(let* ([var (debug (symbol->string 'var) val)] ...)
           exp ...)]))
  
  (define-syntax (letrec-debug stx)
    (syntax-case stx ()
      [(_ ([var val] ...) exp ...)
       #'(letrec ([var (debug (symbol->string 'var) val)] ...)
           exp ...)]))

  )
