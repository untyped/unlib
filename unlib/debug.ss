#lang scheme/base

(require (for-syntax scheme/base
                     scheme/require-transform
                     scheme/provide-transform
                     (only-in "debug-internal.ss" debug))
         scheme/require-syntax
         scheme/provide-syntax
         "base.ss"
         "debug-internal.ss")

; Syntax -----------------------------------------

; symbol symbol -> boolean
(define-for-syntax (symbol<? sym1 sym2)
  (string<? (symbol->string sym1)
            (symbol->string sym2)))

; (_ require-spec)
(define-require-syntax (debug-in stx)
  (syntax-case stx ()
    [(_ msg expr)
     (let ([message (syntax->datum #'msg)])
       (if (string? message)
           (let-values ([(imports sources) (expand-import #'expr)])
             (debug message (sort (map import-src-sym imports) symbol<?)))
           (raise-syntax-error #f "expected string literal" stx #'msg))
       #'expr)]))

; (_ provide-spec)
(define-provide-syntax (debug-out stx)
  (syntax-case stx ()
    [(_ msg expr)
     (let ([message (syntax->datum #'msg)])
       (if (string? message)
           (let ([exports (expand-export #'expr null)])
             (debug message (sort (map export-out-sym exports) symbol<?)))
           (raise-syntax-error #f "expected string literal" stx #'msg))
       #'expr)]))

; Provide statements -----------------------------

(provide (all-from-out "debug-internal.ss")
         debug-in
         debug-out)
