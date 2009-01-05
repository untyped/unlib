#lang scheme/base

(require (for-syntax scheme/base)
         (file "base.ss"))

; (any -> boolean) string -> (any -> any)
;
; Makes a procedure that takes a single argument as a parameter and
; checks it against a predicate. If it matches, it returns the value.
; If not, it throws an exception. Useful as a guard procedure for a
; parameter.
(define (make-guard pred type-message)
  (lambda (val)
    (if (pred val)
        val
        (raise-exn exn:fail:contract 
          (format "Expected ~a, received ~s" type-message val)))))

; syntax (define-parameter identifier any (any -> any) identifier)
(define-syntax (define-parameter stx)
  (syntax-case stx ()
    [(_ id initial-value guard with-form)
     #'(begin (define id 
                (make-parameter initial-value guard))
              (define-syntax (with-form stx)
                (syntax-case stx ()
                  [(with-form new-value exp (... ...))
                   #'(parameterize ([id new-value])
                       exp (... ...))])))]))

; Provide statements -----------------------------

(provide make-guard
         define-parameter)
