#lang scheme/base

(require "test-base.ss"
         "yield.ss")

; Tests -------------------------------------------

(define yield-tests
  (test-suite "yield.ss"
    
    (test-case "a procedure that does not use yield can be called more than once"
      (let ([proc (make-yieldable
                   (lambda (yield)
                     (lambda ()
                       123)))])
        (check-equal? (proc) 123 "check 1")
        (check-equal? (proc) 123 "check 2")))
    
    (test-case "a procedure that uses yield can be called more than once"
      (let ([proc (make-yieldable
                   (lambda (yield)
                     (lambda ()
                       (yield 123)
                       (yield 234)
                       345)))])
        (check-equal? (proc) 123 "check 1")
        (check-equal? (proc) 234 "check 2")
        (check-equal? (proc) 345 "check 3")
        (check-equal? (proc) 123 "check 4")
        (check-equal? (proc) 234 "check 5")
        (check-equal? (proc) 345 "check 6")))
    
    (test-case "yieldable procedures can take more than one argument"
      (let ([proc (make-yieldable
                   (lambda (yield)
                     (lambda (a b)
                       (define-values (c d)
                         (yield (list a b)))
                       (vector c d))))])
        (check-equal? (proc 1 2) (list 1 2)   "check 1")
        (check-equal? (proc 3 4) (vector 3 4) "check 2")
        (check-equal? (proc 5 6) (list 5 6)   "check 3")
        (check-equal? (proc 7 8) (vector 7 8) "check 4")))
    
    (test-case "yieldable procedures can return more than one value"
      (let ([proc (make-yieldable
                   (lambda (yield)
                     (lambda (a b)
                       (define-values (c d)
                         (yield a b))
                       (values c d))))])
        (check-equal? (call-with-values (cut proc 1 2) list) (list 1 2) "check 1")
        (check-equal? (call-with-values (cut proc 3 4) list) (list 3 4) "check 2")
        (check-equal? (call-with-values (cut proc 5 6) list) (list 5 6) "check 3")
        (check-equal? (call-with-values (cut proc 7 8) list) (list 7 8) "check 4")))
    
    (test-case "yieldable syntax works as expected"
      (let ([proc (yieldable yield
                    (lambda ()
                      (yield 123)
                      (yield 234)
                      345))])
        (check-equal? (proc) 123 "check 1")
        (check-equal? (proc) 234 "check 2")
        (check-equal? (proc) 345 "check 3")
        (check-equal? (proc) 123 "check 4")
        (check-equal? (proc) 234 "check 5")
        (check-equal? (proc) 345 "check 6")))))

; Provide statements -----------------------------

(provide yield-tests)
