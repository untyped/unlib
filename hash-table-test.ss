#lang mzscheme

(require (only mzlib/list sort))

(require (file "hash-table.ss")
         (file "test-base.ss"))

(provide hash-table-tests)

(define hash-table-tests
  (test-suite "hash-table.ss"
    
    (test-case "make-hash-table/pairs"
      (let ([hash (make-hash-table/pairs '(a . 1) '(b . (1 2 3)) '(c . ()))])
        (check-equal? (hash-table-get hash 'a #f) 1)
        (check-equal? (hash-table-get hash 'b #f) '(1 2 3))
        (check-equal? (hash-table-get hash 'c #f) null)
        (check-equal? (hash-table-get hash 'd #f) #f)))
    
    (test-case "hash-table-mapped?"
      (let ([hash (make-hash-table/pairs '(a . 1) '(b . (1 2 3)))])
        (check equal? (hash-table-mapped? hash 'a) #t)
        (check equal? (hash-table-mapped? hash 'b) #t)
        (check equal? (hash-table-mapped? hash 'c) #f)))
    
    (test-case "hash-table-keys and hash-table-values"
      (let ([hash (make-hash-table/pairs '(1 . "3") '(2 . "2") '(3 . "1"))])
        (check-equal? (sort (hash-table-keys hash)   <)        (list 1 2 3)       "keys")
        (check-equal? (sort (hash-table-values hash) string<?) (list "1" "2" "3") "values")))

    ))
