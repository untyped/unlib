#lang mzscheme

(require mzlib/etc)

(require (file "debug.ss"))

; Procedures -----------------------------------

; syntax define-traced
;
; Accepts one of the following forms:
;
;   (define-traced (my-proc a b c ...) ...)
;   (define-traced my-proc (lambda (a b c ...) ...))
;   (define-traced my-proc (opt-pambda (a b c ...) ...))
;
; Inserts a call to dynamic-wind such that trace messages get printed
; to the console whenever control passes into or out of the procedure.
(define-syntax (define-traced stx)
  ; boolean syntax-object -> syntax-object
  ;
  ; Creates a lambda function for printing an entry or exit message.
  (define (make-message entry? header-stx)
    (datum->syntax-object
     stx
     (let ([header-list (syntax-object->datum header-stx)])
       `(lambda () 
          ; Display "> (" or "< (" 
          (display ,(if entry? "> (" "< ("))
          ; Display procedure name
          (display ,(string-append (symbol->string (car header-list))))
          ; Display argument list
          ,@(let loop ([rest (cdr header-list)])
              (cond
                [(null? rest) null]
                [(symbol? (car rest))
                 (cons `(display " ")
                       (cons `(display ,(car rest))
                             (loop (cdr rest))))]
                [(list? (car rest))
                 (cons `(display " ")
                       (cons `(display ,(caar rest))
                             (cons `(display "*")
                                   (loop (cdr rest)))))]
                [else (error "Bad syntax in function header: " header-list)]))
          ; Display closing paren
          (display ")\n")))))
  (syntax-case stx ()
    [(_ (a1 a2 ...) e1 e2 ...)
     (with-syntax
         ([entry-message (make-message #t (syntax (a1 a2 ...)))]
          [exit-message (make-message #f (syntax (a1 a2 ...)))])
       (syntax
        (define (a1 a2 ...)
          (dynamic-wind
           entry-message
           (lambda ()
             e1 e2 ...)
           exit-message))))]
    [(_ a1 (lambda (a2 a3 ...) e1 e2 ...))
     (with-syntax
         ([entry-message (make-message #t (syntax (a1 a2 a3 ...)))]
          [exit-message (make-message #f (syntax (a1 a2 a3 ...)))])
       (syntax
        (define a1 
          (lambda (a2 a3 ...)
            (dynamic-wind
             entry-message
             (lambda ()
               e1 e2 ...)
             exit-message)))))]
    [(_ a1 (opt-lambda (a2 a3 ...) e1 e2 ...))
     (with-syntax
         ([entry-message (make-message #t (syntax (a1 a2 a3 ...)))]
          [exit-message (make-message #f (syntax (a1 a2 a3 ...)))])
       (syntax
        (define a1 
          (opt-lambda (a2 a3 ...)
            (dynamic-wind
             entry-message
             (lambda ()
               e1 e2 ...)
             exit-message)))))]))

(define-syntax (lambda-traced stx)
  (syntax-case stx ()
    [(lambda-traced (arg ...) exp ...)
     #'(lambda (arg ...)
         (debug "Entering traced lambda" (list arg ...))
         (debug "Leaving traced lambda" (begin exp ...)))]))

; Provide statements --------------------------- 

(provide define-traced
         lambda-traced)
