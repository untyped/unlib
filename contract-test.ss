#lang scheme/base

(require scheme/contract
         (file "contract.ss")
         (file "test-base.ss"))

(provide contract-tests)

(define contract-tests
  (test-suite "contract.ss"
    
    (test-case "arity/c : fixed arity procedures"
      (check-false (contract-first-order-passes? (arity/c 1) (lambda () #t)))
      (check-true  (contract-first-order-passes? (arity/c 1) (lambda (a) #t)))
      (check-false (contract-first-order-passes? (arity/c 1) (lambda (a b) #t))))
    
    (test-case "arity/c : rest arguments"
      (check-false (contract-first-order-passes? (arity/c 2) (lambda (a b c . d) #t)))
      (check-true  (contract-first-order-passes? (arity/c 2) (lambda (a b . c) #t)))
      (check-true  (contract-first-order-passes? (arity/c 2) (lambda (a . b) #t)))
      (check-true  (contract-first-order-passes? (arity/c 2) (lambda a #t))))
    
    (test-case "arity/c : optional arguments"
      (check-false (contract-first-order-passes? (arity/c 2) (lambda (a b c) #t)))
      (check-true  (contract-first-order-passes? (arity/c 2) (lambda (a b [c #f]) #t)))
      (check-true  (contract-first-order-passes? (arity/c 2) (lambda (a [b #f] [c #f]) #t)))
      (check-true  (contract-first-order-passes? (arity/c 2) (lambda ([a #f] [b #f] [c #f]) #t))))
    
    (test-case "arity/c : keyword arguments"
      ; Basically, arity/c doesn't work with keyword procedures
      (check-false (contract-first-order-passes? (arity/c 2) (lambda (#:a a #:b b #:c c) #t)))
      (check-false (contract-first-order-passes? (arity/c 2) (lambda (#:a a #:b b #:c [c #f]) #t)))
      (check-false (contract-first-order-passes? (arity/c 2) (lambda (#:a a #:b [b #f] #:c [c #f]) #t)))
      (check-false (contract-first-order-passes? (arity/c 2) (lambda (#:a [a #f] #:b [b #f] #:c [c #f]) #t)))
      (check-false (contract-first-order-passes? (arity/c 2) (lambda (x #:a a #:b b #:c c) #t)))
      (check-true (contract-first-order-passes? (arity/c 2) (lambda (x y #:a a #:b b #:c c) #t)))
      (check-false (contract-first-order-passes? (arity/c 2) (lambda (x y z #:a a #:b b #:c c) #t)))
      (check-true (contract-first-order-passes? (arity/c 2) (lambda (x y [z #f] #:a a #:b b #:c c) #t))))
    
    ))
