#lang scheme/base

(require "base.ss")

; Procedures -------------------------------------

; (listof stage) target arg ... -> result
;
; where stage    : continue arg ... -> result
;       continue : arg ... -> result
;       target   : arg ... -> result
;       arg      : any
;       result   : any
; 
; Calls a procedure via a pipeline. The result returned is either 
; the result of the procedure or that of the last stage invoked.
(define (call-with-pipeline pipeline procedure . args)
  (define (pipe pipeline . args)
    (if (null? pipeline)
        (apply procedure args)
        (let ([stage (car pipeline)]
              [success 
               (lambda args
                 (apply pipe (cdr pipeline) args))])
          (apply stage (cons success args)))))
  (apply pipe pipeline args))

; Provide statements --------------------------- 

(provide/contract
 [call-with-pipeline (->* ((listof procedure?) procedure?) () #:rest any/c any)])
