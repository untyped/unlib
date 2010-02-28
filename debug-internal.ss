#lang scheme/base

(require (for-syntax scheme/base
                     scheme/require-transform
                     scheme/provide-transform
                     (only-in srfi/13 string-index-right)
                     "base.ss"
                     "syntax.ss")
         scheme/require-syntax
         scheme/path
         scheme/provide-syntax
         (only-in srfi/1 take-right)
         srfi/13
         "base.ss"
         "contract.ss")

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

; string syntax -> any
(define (debug-syntax message value)
  (debug message (syntax->datum value))
  value)

; (_ string any ...) -> any
;
; Prints the value of the specified expression and returns it transparently.
(define-syntax debug*
  (syntax-rules ()
    [(_ message proc arg ...)
     (let ([value (proc arg ...)])
       (when (debug-enabled?)
         ((current-debug-printer) message value))
       value)]))

; (_) -> void
; (_ any) -> any
(define-syntax (debug-location stx)
  (syntax-case stx ()
    [(_)
     #`(when (debug-enabled?)
         (display "Reached ")
         (display #,(syntax-location-string stx))
         (newline))]
    [(_ expr)
     #`(debug (format "Reached ~a" #,(syntax-location-string stx)) expr)]))

; (_ id value)
(define-syntax (define/debug stx)
  
  (define (args->exprs arg-stx)
    (syntax-case arg-stx ()
      [() null]
      [(id rest ...)
       (identifier? #'id)
       (cons #'(list 'id id)
             (args->exprs #'(rest ...)))]
      [([id expr] rest ...)
       (identifier? #'id)
       (cons #'(list 'id id)
             (args->exprs #'(rest ...)))]
      [(kw rest ...)
       (keyword? (syntax->datum #'kw))
       (args->exprs #'(rest ...))]
      [_ (raise-syntax-error #f "bad function argument" arg-stx stx)]))
  
  (syntax-case stx ()
    [(_ (id arg ...)
        expr ...)
     (identifier? #'id)
     #`(define (id arg ...)
         (debug (format ">>> ~a" 'id)
                (list #,@(args->exprs #'(arg ...))))
         (debug (format "<<< ~a"  'id)
                ((lambda () expr ...))))]
    [(_ id expr)
     (identifier? #'id)
     #`(define id (debug (symbol->string 'id) expr))]))

; (_ (id ...) value)
(define-syntax (define-values/debug stx)
  (syntax-case stx ()
    [(_ (id ...) val)
     #`(define-values (id ...) 
         (apply values (debug (format "~a" '(id ...))
                              (call-with-values (lambda () val) list))))]))

; (_ ([id value] ...) expr ...)
(define-syntax (let/debug stx)
  (syntax-case stx ()
    [(_ loop ([var val] ...) exp ...)
     (identifier? #'loop)
     #'(let loop ([var val] ...)
         (debug (symbol->string 'var) val) ...
         exp ...)]
    [(_ ([var val] ...) exp ...)
     #'(let ([var (debug (symbol->string 'var) val)] ...)
         exp ...)]))

; (_ ([id value] ...) expr ...)
(define-syntax (let*/debug stx)
  (syntax-case stx ()
    [(_ ([var val] ...) exp ...)
     #'(let* ([var (debug (symbol->string 'var) val)] ...)
         exp ...)]))

; (_ ([id value] ...) expr ...)
(define-syntax (letrec/debug stx)
  (syntax-case stx ()
    [(_ ([var val] ...) exp ...)
     #'(letrec ([var (debug (symbol->string 'var) val)] ...)
         exp ...)]))

; (_ ([(id ...) value] ...) expr ...)
(define-syntax (let-values/debug stx)
  (syntax-case stx ()
    [(_ ([(var ...) val] ...) exp ...)
     #`(let-values ([(var ...)
                     (apply values (debug (format "~a" '(var ...))
                                          (call-with-values (lambda () val) list)))]
                    ...)
         exp ...)]))

; (_ ([(id ...) value] ...) expr ...)
(define-syntax (let*-values/debug stx)
  (syntax-case stx ()
    [(_ ([(var ...) val] ...) exp ...)
     #`(let*-values ([(var ...)
                      (apply values (debug (format "~a" '(var ...))
                                           (call-with-values (lambda () val) list)))]
                     ...)
         exp ...)]))

; (_ ([(id ...) value] ...) expr ...)
(define-syntax (letrec-values/debug stx)
  (syntax-case stx ()
    [(_ ([(var ...) val] ...) exp ...)
     #`(letrec-values ([(var ...)
                      (apply values (debug (format "~a" '(var ...))
                                           (call-with-values (lambda () val) list)))]
                     ...)
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

; Deprecated aliases -----------------------------

(define-syntax-rule (define-debug . any)        (define/debug . any))
(define-syntax-rule (define-values-debug . any) (define-values/debug . any))
(define-syntax-rule (let-debug . any)           (let/debug . any))
(define-syntax-rule (let*-debug . any)          (let*/debug . any))
(define-syntax-rule (letrec-debug . any)        (letrec/debug . any))
(define-syntax-rule (let-values-debug . any)    (let-values/debug . any))
(define-syntax-rule (let*-values-debug . any)   (let*-values/debug . any))
(define-syntax-rule (letrec-values-debug . any) (letrec-values/debug . any))

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

(provide debug*
         debug-location
         define/debug
         define-values/debug
         let/debug
         let*/debug
         letrec/debug
         let-values/debug
         let*-values/debug
         letrec-values/debug
         define-debug
         define-values-debug
         let-debug
         let*-debug
         letrec-debug
         let-values-debug
         let*-values-debug
         letrec-values-debug
         with-pretty-indent)

(provide/contract
 [debug-enabled?        (parameter/c boolean?)]
 [current-debug-printer (parameter/c (-> string? any/c void?))]
 [debug                 (-> string? any/c any)]
 [debug-syntax          (-> string? syntax? syntax?)]
 [exn-context           (-> exn? (listof symbol?))])
