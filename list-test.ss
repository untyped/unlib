(module list-test mzscheme
  
  (require (lib "cut.ss" "srfi" "26"))
  
  (require (file "list.ss")
           (file "test-base.ss"))
  
  (provide list-tests)
  
  (define list-tests
    (test-suite
     "All tests for list"
     
     (test-case
      "list-swap swaps two non-adjacent list items"
      (check-equal? (list-swap (list 1 2 3 4 5) 1 3) (list 1 4 3 2 5) "non-adjacent items")
      (check-equal? (list-swap (list 1 2 3 4 5) 3 1) (list 1 4 3 2 5) "indices reversed")
      (check-equal? (list-swap (list 1 2 3 4 5) 1 2) (list 1 3 2 4 5) "adjacent items")
      (check-equal? (list-swap (list 1 2 3 4 5) 0 1) (list 2 1 3 4 5) "first two items")
      (check-equal? (list-swap (list 1 2 3 4 5) 3 4) (list 1 2 3 5 4) "last two items"))
     
     (test-case
      "list-swap fails if indices are the same"
      (check-exn exn:fail:contract? (cut list-swap (list 1 2 3 4 5) 0 0)))
     
     (test-case
      "list-swap fails if indices are out of list bound"
      (check-exn exn:fail:contract? (cut list-swap (list 1 2 3 4 5) -1 0))
      (check-exn exn:fail:contract? (cut list-swap (list 1 2 3 4 5)  4 5)))
     
     (test-equal?
      "list-delimit produces null for a null argument list"
      (list-delimit null " ")
      null)

     (test-equal?
      "list-delimit produces a single item for an argument list of length 1"
      (list-delimit '("a") " ")
      '("a"))

     (test-equal?
      "list-delimit produces an interwoven list for an argument list of length > 1"
      (list-delimit '("a" "b" "c") " ")
      '("a" " " "b" " " "c"))
     
     (test-equal?
      "merge-sorted-lists works on odd and even numbers"
      (merge-sorted-lists '(1 3 5 7 9) '(2 4 6 8 10) = <)
      '(1 2 3 4 5 6 7 8 9 10))
     
     (test-equal?
      "merge-sorted-lists removes duplicates"
      (merge-sorted-lists '(1 2 3 4 5) '(3 4 5 6 7) = <)
      '(1 2 3 4 5 6 7))
     
     (test-equal?
      "merge-sorted-lists works in a more general case"
      (merge-sorted-lists '(1 1 5 7 8 8 9 10) '(1 3 3 15 16 17 18) = <)
      '(1 3 5 7 8 9 10 15 16 17 18))

     (test-equal?
      "char-iota produces a list of lowercase letters"
      (char-iota 26)
      (list #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
     
     (test-equal?
      "char-iota produces a list of uppercase letters"
      (char-iota 26 #\A)
      (list #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
     
     (test-equal?
      "char-iota produces a list of digits"
      (char-iota 10 #\0)
      (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
     
     (test-equal?
      "list-pad pads with #f values by default"
      (list-pad (list 1 2 3 4) 7)
      (list #f #f #f 1 2 3 4))
     
     (test-equal?
      "list-pad pads with supplied values"
      (list-pad (list 1 2 3 4) 7 #t)
      (list #t #t #t 1 2 3 4))
     
     (test-case
      "list-pad does not pad if target length is <= list length"
      (check-equal? (list-pad (list 1 2 3 4) 4) (list 1 2 3 4))
      (check-equal? (list-pad (list 1 2 3 4) 3) (list 1 2 3 4)))
     
     (test-equal?
      "list-pad-right pads with #f values by default"
      (list-pad-right (list 1 2 3 4) 7)
      (list 1 2 3 4 #f #f #f))

     (test-equal?
      "list-pad-right pads with supplied values"
      (list-pad-right (list 1 2 3 4) 7 #t)
      (list 1 2 3 4 #t #t #t))
     
     (test-case
      "list-pad-right does not pad if target length is <= list length"
      (check-equal? (list-pad-right (list 1 2 3 4) 4) (list 1 2 3 4))
      (check-equal? (list-pad-right (list 1 2 3 4) 3) (list 1 2 3 4)))
     
     ; Tree iterators ----------------------------
     
     (test-case
      "tree-map works on various structures"
      (check-equal?
       (tree-map (cut * <> 2) 1)
       2)
      (check-equal?
       (tree-map (cut * <> 2) (cons 1 2))
       (cons 2 4))
      (check-equal?
       (tree-map (cut * <> 2) '(1 2 3))
       '(2 4 6))
      (check-equal?
       (tree-map (cut * <> 2) '((1 2) (3 4)))
       '((2 4) (6 8)))
      (check-equal?
       (tree-map (cut * <> 2) '(1 2 3 . 4))
       '(2 4 6 . 8))
      (check-equal?
       (tree-map (cut * <> 2) '((1 (2)) (3) . ((4))))
       '((2 (4)) (6) . ((8)))))
     
     (test-case
      "tree-for-each works on various structures"
      (let* ([ans null]
             [add! 
              (lambda (item) 
                (set! ans (append ans (list item))))])
        (tree-for-each add! 1)
        (check-equal? ans '(1))
        (set! ans null)
        (tree-for-each add! (cons 1 2))
        (check-equal? ans '(1 2))
        (set! ans null)
        (tree-for-each add! '(1 2 3))
        (check-equal? ans '(1 2 3))
        (set! ans null)
        (tree-for-each add! '((1 2) (3 4)))
        (check-equal? ans '(1 2 3 4))
        (set! ans null)
        (tree-for-each add! '(1 2 3 . 4))
        (check-equal? ans `(1 2 3 4))
        (set! ans null)
        (tree-for-each add! '((1 (2)) (3) . ((4))))
        (check-equal? ans '(1 2 3 4))
        (set! ans null)))
     
     ; Association list utilities ----------------
     
     (test-case
      "assoc-value works as expected"
      (let ([test '((key1 . "Value 1")
                    (key2 . "Value 2")
                    (key3 . "Value 3"))])
        (check equal? (assoc-value 'key1 test) "Value 1")
        (check equal? (assoc-value 'key2 test) "Value 2")
        (check equal? (assoc-value 'key3 test) "Value 3")
        (check-exn exn:fail:unlib? (lambda () (assoc-value 'key4 test)))))
     
     (test-case
      "assoc-value/default works as expected"
      (let ([test '((key1 . "Value 1")
                    (key2 . "Value 2")
                    (key3 . "Value 3"))])
        (check equal? (assoc-value/default 'key1 test #f) "Value 1" "Failed check 1")
        (check equal? (assoc-value/default 'key2 test #f) "Value 2" "Failed check 2")
        (check equal? (assoc-value/default 'key3 test #f) "Value 3" "Failed check 3")
        (check equal? (assoc-value/default 'key4 test #f) #f        "Failed check 4")))

     (test-case
      "alist-accessor works as expected"
      (let ([get-value
             (alist-accessor
              '((key1 . "Value 1")
                (key2 . "Value 2")
                (key3 . "Value 3")))])
        (check equal? (get-value 'key1) "Value 1")
        (check equal? (get-value 'key2) "Value 2")
        (check equal? (get-value 'key3) "Value 3")
        (check-exn exn:fail:unlib? (lambda () (get-value 'key4)))))
     
     (test-case
      "alist-accessor/default works as expected"
      (let ([get-value
             (alist-accessor/default
              '((key1 . "Value 1")
                (key2 . "Value 2")
                (key3 . "Value 3"))
              #f)])
        (check equal? (get-value 'key1) "Value 1" "Failed check 1")
        (check equal? (get-value 'key2) "Value 2" "Failed check 2")
        (check equal? (get-value 'key3) "Value 3" "Failed check 3")
        (check equal? (get-value 'key4) #f        "Failed check 4")))

     (test-case
      "alist-set works as expected"
      (let ([test1 null]
            [test2 null]
            [test3 null])
        (set! test1 (alist-set 'key1 "Value 1" null))
        (check equal? (assoc-value/default 'key1 test1 #f) "Value 1" "Failed check 1a")
        (check equal? (assoc-value/default 'key1 test2 #f) #f        "Failed check 1b")
        (check equal? (assoc-value/default 'key1 test3 #f) #f        "Failed check 1c")
        (check equal? (assoc-value/default 'key2 test1 #f) #f        "Failed check 1d")
        (check equal? (assoc-value/default 'key2 test2 #f) #f        "Failed check 1e")
        (check equal? (assoc-value/default 'key2 test3 #f) #f        "Failed check 1f")
        (check equal? (assoc-value/default 'key3 test1 #f) #f        "Failed check 1g")
        (check equal? (assoc-value/default 'key3 test2 #f) #f        "Failed check 1h")
        (check equal? (assoc-value/default 'key3 test3 #f) #f        "Failed check 1i")
        (set! test2 (alist-set 'key2 "Value 2" test1))
        (check equal? (assoc-value/default 'key1 test1 #f) "Value 1" "Failed check 2a")
        (check equal? (assoc-value/default 'key1 test2 #f) "Value 1" "Failed check 2b")
        (check equal? (assoc-value/default 'key1 test3 #f) #f        "Failed check 2c")
        (check equal? (assoc-value/default 'key2 test1 #f) #f        "Failed check 2d")
        (check equal? (assoc-value/default 'key2 test2 #f) "Value 2" "Failed check 2e")
        (check equal? (assoc-value/default 'key2 test3 #f) #f        "Failed check 2f")
        (check equal? (assoc-value/default 'key3 test1 #f) #f        "Failed check 2g")
        (check equal? (assoc-value/default 'key3 test2 #f) #f        "Failed check 2h")
        (check equal? (assoc-value/default 'key3 test3 #f) #f        "Failed check 2i")
        (set! test3 (alist-set 'key3 "Value 3" test2))
        (check equal? (assoc-value/default 'key1 test1 #f) "Value 1" "Failed check 3a")
        (check equal? (assoc-value/default 'key1 test2 #f) "Value 1" "Failed check 3b")
        (check equal? (assoc-value/default 'key1 test3 #f) "Value 1" "Failed check 3c")
        (check equal? (assoc-value/default 'key2 test1 #f) #f        "Failed check 3d")
        (check equal? (assoc-value/default 'key2 test2 #f) "Value 2" "Failed check 3e")
        (check equal? (assoc-value/default 'key2 test3 #f) "Value 2" "Failed check 3f")
        (check equal? (assoc-value/default 'key3 test1 #f) #f        "Failed check 3g")
        (check equal? (assoc-value/default 'key3 test2 #f) #f        "Failed check 3h")
        (check equal? (assoc-value/default 'key3 test3 #f) "Value 3" "Failed check 3i")))

     (test-case
      "alist-mutator works as expected"
      (let* ([test null]
             [set-value! (alist-mutator test)])
        (set-value! 'key1 "Value 1")
        (check equal? (assoc-value/default 'key1 test #f) "Value 1" "Failed check 1a")
        (check equal? (assoc-value/default 'key2 test #f) #f        "Failed check 1b")
        (check equal? (assoc-value/default 'key3 test #f) #f        "Failed check 1c")
        (set-value! 'key2 "Value 2")
        (check equal? (assoc-value/default 'key1 test #f) "Value 1" "Failed check 2a")
        (check equal? (assoc-value/default 'key2 test #f) "Value 2" "Failed check 2b")
        (check equal? (assoc-value/default 'key3 test #f) #f        "Failed check 2c")
        (set-value! 'key3 "Value 3")
        (check equal? (assoc-value/default 'key1 test #f) "Value 1" "Failed check 3a")
        (check equal? (assoc-value/default 'key2 test #f) "Value 2" "Failed check 3b")
        (check equal? (assoc-value/default 'key3 test #f) "Value 3" "Failed check 3c")))

     (test-case
      "alist-mutator/append works as expected"
      (let* ([test null]
             [get-value (alist-accessor/default test null)]
             [add-value! (alist-mutator/append test)])
        (add-value! 'key1 "Value 1")
        (check equal? (get-value 'key1) '("Value 1")           "Failed check 1a")
        (check equal? (get-value 'key2) null                   "Failed check 1b")
        (check equal? (get-value 'key3) null                   "Failed check 1c")
        (add-value! 'key2 "Value 2")
        (check equal? (get-value 'key1) '("Value 1")           "Failed check 2a")
        (check equal? (get-value 'key2) '("Value 2")           "Failed check 2b")
        (check equal? (get-value 'key3) null                   "Failed check 2c")
        (add-value! 'key1 "Value 3")
        (check equal? (get-value 'key1) '("Value 1" "Value 3") "Failed check 3a")
        (check equal? (get-value 'key2) '("Value 2")           "Failed check 3b")
        (check equal? (get-value 'key3) null                   "Failed check 3c")))

     (test-case
      "alist-map works as expected"
      (check equal?
             (alist-map
              string-append
              '(("a" . "1")
                ("b" . "2")
                ("c" . "3")))
             '("a1" "b2" "c3")))

     (test-case
      "alist-map throws an exception on non-pair elements"
      (check-exn
       exn:fail:unlib?
       (lambda ()
         (alist-map
          string-append
          '(("a" . "1")
            "b"
            ("c" . "3"))))))

     (test-case
      "alist-for-each works as expected"
      (let ([keys ""] [values ""])
         (alist-for-each
          (lambda (key value)
            (set! keys (string-append keys key))
            (set! values (string-append values value)))
          '(("a" . "1")
            ("b" . "2")
            ("c" . "3")))
        (check equal? keys "abc")
        (check equal? values "123")))
            
     (test-case
      "alist-for-each throws an exception on non-pair elements"
      (check-exn
       exn:fail:unlib?
       (lambda ()
         (alist-for-each
          (lambda (key value)
            (format "~a:~a~n" key value))
          '(("a" . "1")
            "b"
            ("c" . "3"))))))
     
     (test-case
      "alist-merge prefers keys from first argument by default"
      (check-equal?
       (alist-merge '((a . 1) 
                      (b . 2)
                      (c . 3))
                    '((b . 4)
                      (c . 5)
                      (d . 6)))
       '((a . 1)
         (b . 2)
         (c . 3)
         (d . 6))))
     
     (test-case
      "alist-merge can be set to prefers keys from second argument"
      (check-equal?
       (alist-merge '((a . 1)
                      (b . 2)
                      (c . 3))
                    '((b . 4)
                      (c . 5)
                      (d . 6))
                    'second)
       '((a . 1) 
         (b . 4)
         (c . 5)
         (d . 6))))

     ))
  )
