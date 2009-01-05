(module hash-table-test mzscheme
  
  (require (file "hash-table.ss")
           (file "test-base.ss"))
  
  (provide hash-table-tests)
  
  (define test-table
    (make-hash-table/pairs
     '(a . 1)
     '(b . 2)
     '(c . 3)))

  (define test-table-2
    (make-hash-table/pairs
     '(a . (1 2 3))
     '(b . (4 5 6))
     '(c . (7 8 9))))

  (define test-table-3
    (make-hash-table/pairs
     '(a . ())
     '(b . ())
     '(c . ())))

  (define hash-table-tests
    (test-suite
     "All tests for hash-table"

     (test-case
      "make-hash-table/pairs works as expected"
      (check equal? (hash-table-get test-table 'a) 1)
      (check equal? (hash-table-get test-table 'b) 2)
      (check equal? (hash-table-get test-table 'c) 3)
      (check equal? (hash-table-get test-table-2 'a) '(1 2 3))
      (check equal? (hash-table-get test-table-2 'b) '(4 5 6))
      (check equal? (hash-table-get test-table-2 'c) '(7 8 9))
      (check equal? (hash-table-get test-table-3 'a) null)
      (check equal? (hash-table-get test-table-3 'b) null)
      (check equal? (hash-table-get test-table-3 'c) null))
     
     (test-case
      "hash-table-mapped? works"
      (let ([table (make-hash-table/pairs
                    (cons 'a 1)
                    (cons 'b #f))])
        (check equal? (hash-table-mapped? table 'a) #t)
        (check equal? (hash-table-mapped? table 'b) #t)
        (check equal? (hash-table-mapped? table 'c) #f)))
     
     (test-case
      "hash-table-accessor works"
      (let ([get-value (hash-table-accessor test-table)])
        (check equal? (get-value 'a) 1)
        (check equal? (get-value 'b) 2)
        (check equal? (get-value 'c) 3)
        (check-exn exn:fail:unlib? (lambda () (get-value 'd)))))
     
     (test-case
      "hash-table-accessor/default works"
      (let ([get-value (hash-table-accessor/default test-table 12345)])
        (check equal? (get-value 'a) 1)
        (check equal? (get-value 'b) 2)
        (check equal? (get-value 'c) 3)
        (check equal? (get-value 'd) 12345)))
     
     (test-case
      "hash-table-mutator works"
      (let* ([table (hash-table-copy test-table)]
             [get-value (hash-table-accessor table)]
             [set-value! (hash-table-mutator table)])
        (check equal? (get-value 'a) 1)
        (check equal? (get-value 'b) 2)
        (check equal? (get-value 'c) 3)
        (check-exn exn:fail:unlib? (lambda () (get-value 'd)))
        (set-value! 'a 3)
        (set-value! 'c 1)
        (set-value! 'd 0)
        (check equal? (get-value 'a) 3)
        (check equal? (get-value 'b) 2)
        (check equal? (get-value 'c) 1)
        (check equal? (get-value 'd) 0)))
     
     (test-case
      "hash-table-mutator/append works"
      (let* ([table (hash-table-copy test-table-2)]
             [get-value (hash-table-accessor table)]
             [add-value! (hash-table-mutator/append table)])
        (check equal? (get-value 'a) '(1 2 3))
        (check equal? (get-value 'b) '(4 5 6))
        (check equal? (get-value 'c) '(7 8 9))
        (check-exn exn:fail:unlib? (lambda () (get-value 'd)))
        (add-value! 'a 4)
        (add-value! 'c 0)
        (add-value! 'd 10)
        (check equal? (get-value 'a) '(1 2 3 4))
        (check equal? (get-value 'b) '(4 5 6))
        (check equal? (get-value 'c) '(7 8 9 0))
        (check equal? (get-value 'd) '(10))))
     
     (test-case
      "hash-table-find finds value correctly"
      (hash-table-find
       test-table
       (lambda (key val)
         (if (even? val)
             (* val 2))))
      4)

     (test-case
      "hash-table-find fails to find value correctly"
      (hash-table-find
       test-table
       (lambda (key val)
         (> val 3)))
      #f)

     (test-case
      "hash-table-find returns default value correctly"
      (hash-table-find
       test-table
       (lambda (key val)
         (if (even? val)
             (* val 2)))
       (lambda () '(1 2 3)))
      '(1 2 3))

     (test-case 
      "any-keys-have-values? returns true appropriately"
      (check equal?
             (any-keys-have-values? test-table-2)
             #t))
     
     (test-case 
      "any-keys-have-values? returns false appropriately"
      (check equal?
             (any-keys-have-values? test-table-3)
             #f))

     (test-case
      "any-keys-have-values? throws exception when key not mapped to list"
      (check-exn 
       exn:fail:unlib?
       (lambda ()
         (any-keys-have-values? test-table))))
     
     (test-case 
      "key-has-values? returns true appropriately"
      (check equal?
             (key-has-values? test-table-2 'b)
             #t))
     
     (test-case 
      "key-has-values? returns false appropriately"
      (check equal?
             (key-has-values? test-table-3 'b)
             #f))

     (test-case
      "key-has-values? throws exception when key not mapped to list"
      (check-exn 
       exn:fail:unlib?
       (lambda ()
         (key-has-values? test-table 'b))))
     
     ))
  )
