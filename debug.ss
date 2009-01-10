#lang scheme/base

(require (for-syntax scheme/base
                     (file "syntax.ss"))
         scheme/contract
         scheme/pretty
         srfi/13/string
         (file "base.ss")
         (file "contract.ss"))

; (parameter boolean)
(define debug-enabled?
  (make-parameter #t))

; string any -> void
(define (default-debug-printer message value)
  (define value-string
    (parameterize ([pretty-print-print-line 
                    (lambda (line-number out old-length num-columns)
                      (display "\n  " out)
                      2)])
      (pretty-format value)))
  (printf "~a:~a" message (string-drop-right value-string 1)))

; (parameter (string any -> void))
(define current-debug-printer
  (make-parameter default-debug-printer))

; string any -> any
;
; Prints the value of the specified expression and returns it transparently.
(define (debug message value)
  (when (debug-enabled?)
    ((current-debug-printer) message value))
  value)

; string procedure any ... -> any
;
; Prints the value of the specified expression and returns it transparently.
(define (debug* message proc . args)
  (define value (apply proc args))
  (when (debug-enabled?)
    ((current-debug-printer) message value))
  value)

; (_ id value)
(define-syntax (define-debug stx)
  (syntax-case stx ()
    [(_ id val)
     #`(define id (debug (symbol->string 'id) val))]))

; (_ ([id value] ...) expr ...)
(define-syntax (let-debug stx)
  (syntax-case stx ()
    [(_ ([var val] ...) exp ...)
     #'(let ([var (debug (symbol->string 'var) val)] ...)
         exp ...)]))

; (_ ([id value] ...) expr ...)
(define-syntax (let*-debug stx)
  (syntax-case stx ()
    [(_ ([var val] ...) exp ...)
     #'(let* ([var (debug (symbol->string 'var) val)] ...)
         exp ...)]))

; (_ ([id value] ...) expr ...)
(define-syntax (letrec-debug stx)
  (syntax-case stx ()
    [(_ ([var val] ...) exp ...)
     #'(letrec ([var (debug (symbol->string 'var) val)] ...)
         exp ...)]))

; Provide statements -----------------------------

(provide define-debug
         let-debug
         let*-debug
         letrec-debug)

(provide/contract
 [debug-enabled?        (parameter/c boolean?)]
 [current-debug-printer (parameter/c (-> string? any/c void?))]
 [debug                 (-> string? any/c any)]
 [debug*                (->* (string? procedure?) () #:rest any/c any)])