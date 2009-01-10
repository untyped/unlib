#lang scheme/base

(require (file "generator.ss")
         (file "hash.ss")
         (file "test-base.ss"))

; Helpers ----------------------------------------

; (hashof a b) -> (alistof a b)
(define (hash->alist hash)
  (sort (hash-map hash cons)
        (lambda (a b)
          (< (car a) (car b)))))

; Tests ------------------------------------------

(define generator-tests
  (test-suite "generator.ss"
    
    (test-case "list->generator"
      (let ([gen (list->generator (list 1 2 3))])
        (check-equal? (gen) 1  "check 1")
        (check-equal? (gen) 2  "check 2")
        (check-equal? (gen) 3  "check 3")
        (check-pred generator-end? (gen) "check 4")
        (check-pred generator-end? (gen) "check 5")))
    
    (test-case "range->generator"
      (check-equal? (generator->list (range->generator 1 5 2)) '(1 3))
      (check-equal? (generator->list (range->generator 0 5 2)) '(0 2 4))
      (check-equal? (generator->list (range->generator 0 5)) '(0 1 2 3 4))
      (check-equal? (generator->list (range->generator 0 -10 -3)) '(0 -3 -6 -9))
      (let ([gen (range->generator 5)])
        (check-equal? (list (gen) (gen) (gen) (gen) (gen) (gen)) '(5 6 7 8 9 10))))
    
    (test-case "generator->list works as expected"
      (check-equal? (generator->list (list->generator (list 1 2 3)))
                    (list 1 2 3))
      (check-equal? (generator->list (list->generator (list 1 2 3 generator-end 4 5)))
                    (list 1 2 3)))
    
    (test-case "generator->hash works as expected"
      (check-equal? (hash->alist
                     (generator->hash
                      (list->generator (list 2 4 6))
                      (cut / <> 2)))
                    '((1 . 2) 
                      (2 . 4)
                      (3 . 6)))
      (check-equal? (hash->alist
                     (generator->hash
                      (list->generator (list 2 4 6))
                      (cut / <> 2)
                      (cut * <> 2)
                      (make-hash/alist '((100 . 200) (300 . 400)))))
                    '((1   . 4) 
                      (2   . 8)
                      (3   . 12)
                      (100 . 200)
                      (300 . 400))))
    
    (test-case "generator-fold works as expected"
      (check-equal? (generator-fold 
                     cons
                     null
                     (list->generator (list 1 2 3)))
                    (list 3 2 1))
      (check-equal? (generator-fold
                     (lambda (a b accum)
                       (cons (+ a b) accum))
                     null
                     (list->generator (list 1 2 3 4 5))
                     (list->generator (list 2 4 6 8 10)))
                    (list 15 12 9 6 3))
      (check-equal? (generator-fold
                     (lambda (a b accum)
                       (cons (+ a b) accum))
                     null
                     (list->generator (list 1 2 3 4 5))
                     (list->generator (list 2 4 6 generator-end 10)))
                    (list 9 6 3)))
    
    (test-case "generator-map works as expected"
      (check-equal? (generator->list 
                     (generator-map 
                      even?
                      (list->generator (list 1 2 3 4 5))))
                    (list #f #t #f #t #f))
      (check-equal? (generator->list
                     (generator-map 
                      +
                      (list->generator (list 1 2 3 4 5))
                      (list->generator (list 1 3 5 7 9))))
                    (list 2 5 8 11 14)))
    
    (test-case "generator-fold-map works as expected"
      (check-equal? (generator->list 
                     (generator-fold-map
                      +
                      0
                      (list->generator (list 1 2 3 4 5))))
                    (list 1 3 6 10 15))
      (check-equal? (generator->list
                     (generator-fold-map 
                      +
                      0
                      (list->generator (list 1 2 3 4 5))
                      (list->generator (list 1 3 5 7 9))))
                    (list 2 7 15 26 40)))
    
    (test-case "generator-filter works as expected"
      (check-equal? (generator->list 
                     (generator-filter
                      even?
                      (list->generator (list 1 2 3 4 5))))
                    (list 2 4)))
    
    (test-case "generator-filter-map works as expected"
      (check-equal? (generator->list 
                     (generator-filter-map
                      (lambda (a)
                        (if (even? a)
                            (* 2 a)
                            #f))
                      (list->generator (list 1 2 3 4 5))))
                    (list 4 8)))
    
    (test-case "generator-remove-duplicates works as expected"
      (check-equal? (generator->list 
                     (generator-remove-duplicates
                      (list->generator (list 1 2 2 3 3 3 4 4 5))))
                    (list 1 2 3 4 5)))
    
    (test-case "generator-for-each works as expected"
      (check-equal? (let ([accum null])
                      (generator-for-each
                       (lambda (a)
                         (set! accum (cons a accum)))
                       (list->generator (list 1 2 3 4 5)))
                      accum)
                    (list 5 4 3 2 1))
      (check-equal? (let ([accum null])
                      (generator-for-each
                       (lambda (a b)
                         (set! accum (cons (+ a b) accum)))
                       (list->generator (list 1 2 3 4 5))
                       (list->generator (list 2 4 6 8 10)))
                      accum)
                    (list 15 12 9 6 3)))
    
    (test-case "generator-project works as expected"
      (let ([generate (generator-project (list #t #t #f #f)
                                         (list->generator (list (list 0 0 0 0)
                                                                (list 0 0 0 1)
                                                                (list 0 0 1 0)
                                                                (list 0 0 1 1)
                                                                (list 0 1 0 0)
                                                                (list 0 1 0 1)
                                                                (list 0 1 1 0)
                                                                (list 0 1 1 1))))])
        (check-equal? (generate) 
                      (list 0 0 (list (list 0 0)
                                      (list 0 1)
                                      (list 1 0)
                                      (list 1 1)))
                      "check 1")
        (check-equal? (generate)
                      (list 0 1 (list (list 0 0)
                                      (list 0 1)
                                      (list 1 0)
                                      (list 1 1)))
                      "check 2")
        (check-pred generator-end?
                    (generate)
                    "check 3")
        (check-pred generator-end?
                    (generate)
                    "check 4")))
    
    (test-case "generator-project with mask completely #t"
      (let ([generate (generator-project (list #t #t #t #t)
                                         (list->generator (list (list 0 0 0 0)
                                                                (list 0 0 0 1)
                                                                (list 0 0 1 0)
                                                                (list 0 0 1 1)
                                                                (list 0 1 0 0)
                                                                (list 0 1 0 1)
                                                                (list 0 1 1 0)
                                                                (list 0 1 1 1))))])
        (check-equal? (generate) (list 0 0 0 0 (list (list))))
        (check-equal? (generate) (list 0 0 0 1 (list (list))))
        (check-equal? (generate) (list 0 0 1 0 (list (list))))
        (check-equal? (generate) (list 0 0 1 1 (list (list))))
        (check-equal? (generate) (list 0 1 0 0 (list (list))))
        (check-equal? (generate) (list 0 1 0 1 (list (list))))
        (check-equal? (generate) (list 0 1 1 0 (list (list))))
        (check-equal? (generate) (list 0 1 1 1 (list (list))))
        (check-pred generator-end? (generate))))
    
    (test-case "generator-project with mask completely #f"
      (let ([generate (generator-project (list #f #f #f #f)
                                         (list->generator (list (list 0 0 0 0)
                                                                (list 0 0 0 1)
                                                                (list 0 0 1 0)
                                                                (list 0 0 1 1)
                                                                (list 0 1 0 0)
                                                                (list 0 1 0 1)
                                                                (list 0 1 1 0)
                                                                (list 0 1 1 1))))])
        (check-equal? (generate) (list (list (list 0 0 0 0)
                                             (list 0 0 0 1)
                                             (list 0 0 1 0)
                                             (list 0 0 1 1)
                                             (list 0 1 0 0)
                                             (list 0 1 0 1)
                                             (list 0 1 1 0)
                                             (list 0 1 1 1))))
        (check-pred generator-end? (generate))))
    
    (test-case "generator-project passes non-list items straight through"
      (let ([generate (generator-project (list #t #t #f #f)
                                         (list->generator (list (list 0 0 0 0)
                                                                (list 0 0 0 1)
                                                                123
                                                                (list 0 1 0 0)
                                                                (list 0 1 0 1)
                                                                456)))])
        (check-equal? (generate) (list 0 0 (list (list 0 0) (list 0 1))) "check 1")
        (check-equal? (generate) 123)
        (check-equal? (generate) (list 0 1 (list (list 0 0) (list 0 1))) "check 2")
        (check-equal? (generate) 456)
        (check-pred generator-end? (generate))))
    
    (test-case "generator-project passes non-list items straight through"
      (let ([generate (generator-project (list #t #t #f #f)
                                         (list->generator (list (list 0 0 0 0)
                                                                (list 0 0 0 1)
                                                                123
                                                                (list 0 1 0 0)
                                                                (list 0 1 0 1)
                                                                456)))])
        (check-equal? (generate) (list 0 0 (list (list 0 0) (list 0 1))) "check 1")
        (check-equal? (generate) 123)
        (check-equal? (generate) (list 0 1 (list (list 0 0) (list 0 1))) "check 2")
        (check-equal? (generate) 456)
        (check-pred generator-end? (generate))))
    
    (test-case "generator-project uses eq? by default"
      (let ([generate (generator-project (list #t #t #f #f)
                                         (list->generator (list (list "0" "0" "0" "0")
                                                                (list "0" "0" "0" "1")
                                                                123
                                                                (list "0" "1" "0" "0")
                                                                (list "0" "1" "0" "1")
                                                                456)))])
        (check-equal? (generate) (list "0" "0" (list (list "0" "0"))) "check 1")
        (check-equal? (generate) (list "0" "0" (list (list "0" "1"))) "check 2")
        (check-equal? (generate) 123 "check 3")
        (check-equal? (generate) (list "0" "1" (list (list "0" "0"))) "check 4")
        (check-equal? (generate) (list "0" "1" (list (list "0" "1"))) "check 5")
        (check-equal? (generate) 456 "check 6")
        (check-pred generator-end? (generate) "check 7")))
    
    (test-case "generator-project can be configured to use equal? instead of eq?"
      (let ([generate (generator-project (list #t #t #f #f)
                                         (list->generator (list (list "0" "0" "0" "0")
                                                                (list "0" "0" "0" "1")
                                                                123
                                                                (list "0" "1" "0" "0")
                                                                (list "0" "1" "0" "1")
                                                                456))
                                         equal?)])
        (check-equal? (generate) (list "0" "0" (list (list "0" "0") (list "0" "1"))) "check 1")
        (check-equal? (generate) 123 "check 2")
        (check-equal? (generate) (list "0" "1" (list (list "0" "0") (list "0" "1"))) "check 3")
        (check-equal? (generate) 456 "check 4")
        (check-pred generator-end? (generate) "check 5")))))

; Provide statements -----------------------------

(provide generator-tests)

