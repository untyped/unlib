#lang scheme/base

(require (file "base.ss")
         (file "number.ss"))

; Contracts ------------------------------------

; natural -> contract
(define (arity/c arity)
  (flat-named-contract
   (format "procedure-with-arity-~a/c" arity)
   (lambda (item)
     (and (procedure? item)
          (procedure-arity-includes? item arity)))))

; Provide statements --------------------------- 

(provide/contract
 [arity/c (-> natural? flat-contract?)])
