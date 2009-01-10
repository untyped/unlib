#lang scheme/base

(require (for-syntax scheme/base
                     (file "syntax.ss"))
         scheme/contract
         scheme/match
         scheme/path
         scheme/pretty
         (only-in srfi/1/list take-right)
         srfi/13/string
         (file "base.ss")
         (file "contract.ss"))

; (parameter boolean)
(define debug-enabled?
  (make-parameter #t))

; string any -> void
(define (default-debug-printer message value)
  ; string
  (define value-string
    (with-pretty-indent "  "
      (pretty-format value)))
  (printf "~a:~n~a~n" message value-string))

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

; string (-> any) -> any
(define-syntax with-pretty-indent
  (syntax-rules ()
    [(_ indent expr ...)
     (let* ([indent-string indent]
            [indent-amount (string-length indent-string)])
       (parameterize ([pretty-print-print-line 
                       (lambda (line out offset width)
                         (cond [(eq? line 0)   (display indent-string out)
                                               indent-amount]
                               [(number? line) (if (number? width)
                                                 (begin
                                                   (newline out)
                                                   (display indent-string out)
                                                   indent-amount)
                                                 (begin 
                                                   0))]
                               [else             (when (number? width)
                                                   (newline out))
                                                 0]))])
         expr ...))]))

; exn -> (listof symbol)
(define (exn-context exn)
  (map (match-lambda
         [(list-rest name srcloc)
          (string->symbol
           (if srcloc
               (format "~a:~a:~a:~a"
                       (path->short-string (srcloc-source srcloc))
                       (srcloc-line srcloc)
                       (srcloc-column srcloc)
                       (or name '??))
               (format "??:??:??:~a"
                       (or name '??))))])
       (continuation-mark-set->context (exn-continuation-marks exn))))

; Helpers ----------------------------------------

; path -> string
(define (path->short-string path)
  ; (listof (U string 'up 'same))
  (define path-elements
    (foldr (lambda (element accum)
             (cond [(path? element)     (cons (path->string element) accum)]
                   [(eq? element 'same) accum]
                   [(eq? element 'up)   (cons ".." accum)]))
           null
           (explode-path (simplify-path path))))
  ; natural
  (define num-elements
    (length path-elements))
  (string-join (take-right path-elements (min 3 num-elements)) "/"))

; Provide statements -----------------------------

(provide define-debug
         let-debug
         let*-debug
         letrec-debug
         with-pretty-indent)

(provide/contract
 [debug-enabled?        (parameter/c boolean?)]
 [current-debug-printer (parameter/c (-> string? any/c void?))]
 [debug                 (-> string? any/c any)]
 [debug*                (->* (string? procedure?) () #:rest any/c any)]
 [exn-context           (-> exn? (listof symbol?))])