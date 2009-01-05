#lang scheme/base

(require scheme/contract
         (file "base.ss"))

; Structure types --------------------------------

; (struct symbol ((a b c ... -> result) a b c ... -> result))
;
; The first argument to the body procedure is a continuation procedure
; that passes control to the next stage in the pipeline.
(define-struct stage (name body)
  #:transparent
  #:property prop:procedure
  (struct-field-index body))

; Syntax -----------------------------------------

; (_ (name continue arg ...) expr ...)
(define-syntax define-stage
  (syntax-rules ()
    [(define-stage (name continue args ...)
       expr ...)
     (define name 
       (make-stage 
        'name
        (lambda (continue args ...)
          expr ...)))]
    [(define-stage (name continue args ... . rest)
       expr ...)
     (define name 
       (make-stage 
        'name
        (lambda (continue args ... . rest)
          expr ...)))]))

; Procedures -------------------------------------

; pipeline (any ... -> any) any ... -> any
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

; (listof stage) symbol -> (U stage #f)
;
; Returns the appropriately named stage in the specified pipeline,
; or #f if such a stage cannot be found.
(define (find-stage pipeline name)
  (ormap (lambda (stage)
           (and (eq? (stage-name stage) name) stage))
         pipeline))

; Provide statements --------------------------- 

(provide define-stage)

(provide/contract
 [struct stage       ([name symbol?] [body procedure?])]
 [call-with-pipeline (->* ((listof stage?) procedure?) () #:rest any/c any)]
 [find-stage         (-> (listof stage?) symbol? (or/c stage? false/c))])
