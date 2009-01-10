#lang scheme/base

(require scheme/list
         "hash.ss"
         "test-base.ss")

; Tests ------------------------------------------

(define hash-tests
  (test-suite "hash.ss"
    
    (test-case "make-hash/alist"
      (let ([hash (make-hash/alist '((a . 1) (b . 2) ("c" . 3)))])
        (check-false (hash-eq? hash) "eq")
        (check-false (hash-weak? hash) "weak")
        (check equal? (hash-ref hash 'a) 1)
        (check equal? (hash-ref hash 'b) 2)
        (check equal? (hash-ref hash "c") 3)))
    
    (test-case "make-hasheq/alist"
      (let ([hash (make-hasheq/alist '((a . 1) (b . 2) ("c" . 3)))])
        (check-true (hash-eq? hash) "eq")
        (check-false (hash-weak? hash) "weak")
        (check equal? (hash-ref hash 'a) 1)
        (check equal? (hash-ref hash 'b) 2)
        (check equal? (hash-ref hash "c" #f) #f)))
    
    (test-case "make-weak-hash/alist"
      (check-false (hash-eq? (make-weak-hash/alist null)) "eq")
      (check-true (hash-weak? (make-weak-hash/alist null)) "weak"))
    
    (test-case "make-weak-hasheq/alist"
      (check-true (hash-eq? (make-weak-hasheq/alist null)) "eq")
      (check-true (hash-weak? (make-weak-hasheq/alist null)) "weak"))
    
    (test-case "hash-set?"
      (let ([hash (make-hasheq/alist '((a . 1) (b . 2)))])
        (check-true (hash-set? hash 'a) "a")
        (check-true (hash-set? hash 'b) "b")
        (check-false (hash-set? hash 'c) "c")))
    
    (test-case "hash-keys and hash-values"
      (let ([hash (make-hasheq/alist '((3 . 1) (2 . 2) (1 . 3)))])
        (check-equal? (sort (hash-keys hash) <) '(1 2 3) "keys")
        (check-equal? (sort (hash-values hash) <) '(1 2 3) "values")))
    
    ))

; Provide statements -----------------------------

(provide hash-tests)
