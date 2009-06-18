#lang scheme/base

(require scheme/contract
         scheme/match
         scheme/pretty
         srfi/26
         "exn.ss"
         "require.ss")

(define-library-aliases cce-scheme (planet cce/scheme:4:1)          #:provide)
(define-library-aliases namespace  (planet schematics/namespace:1)  #:provide)
(define-library-aliases schemeunit (planet schematics/schemeunit:3) #:provide)

; Provide statements --------------------------- 

(provide (all-from-out scheme/contract
                       scheme/match
                       scheme/pretty
                       srfi/26
                       "exn.ss"))
