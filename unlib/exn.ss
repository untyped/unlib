#lang scheme/base

(require (for-syntax scheme/base
                     "syntax.ss"))

; (_ exception string)
(define-syntax (raise-exn stx)
  (syntax-case stx ()
    [(_ exception message extra-args ...)
     (with-syntax ([make-proc (make-id stx 'make- (syntax exception))])
       #'(raise (apply make-proc (list (string->immutable-string message)
                                       (current-continuation-marks)
                                       extra-args ...))))]))

; (_ exn exn string any ...)
(define-syntax (reraise-exn stx)
  (syntax-case stx ()
    [(_ old-exn new-exn message constructor-args ...)
     (with-syntax ([make-proc (make-id #'new-exn 'make- (syntax new-exn))])
       #'(raise (make-proc (string-append message ": " (exn-message old-exn))
                           (exn-continuation-marks old-exn)
                           constructor-args ...)))]))

; Provide statements --------------------------- 

(provide raise-exn
         reraise-exn)
