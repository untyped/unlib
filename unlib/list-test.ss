#lang scheme/base

(require (file "list.ss")
         (file "test-base.ss"))

(define list-tests
  (test-suite "list.ss"
    
    (test-case "make-list*"
      (check-equal? (make-list* 10 '(1)) '(1 1 1 1 1 1 1 1 1 1))
      (check-equal? (make-list* 10 '(1 2)) '(1 2 1 2 1 2 1 2 1 2))
      (check-equal? (make-list* 10 '(1 2 3)) '(1 2 3 1 2 3 1 2 3 1))
      (check-exn exn:fail:contract? (cut make-list* 10 null))
      (check-exn exn:fail:contract? (cut make-list* -1 '(1 2 3)))
      (check-equal? (make-list* 0 '(1 2 3)) null))
    
    (test-case "assemble-list"
      (let ([x 1]
            [y 2]
            [z 3]
            [a (list 10 20 30)]
            [b (list 40 50 60)]
            [c (list 70 80 90)])
        (check-equal? (assemble-list [#t x y z] 
                                     [#f z y x]
                                     [x  4 5 6]
                                     [#t ,@a]
                                     [#f ,@b]
                                     [x  ,@c])
                      (list 1 2 3 4 5 6 10 20 30 70 80 90))))
    
    (test-case "in-list/cycle"
      (check-equal? (for/list ([i (in-list '(1 2 3 4 5 6 7))] [j (in-list/cycle '(1 2 3))]) j)
                    '(1 2 3 1 2 3 1))
      (check-equal? (for/list ([i (in-list '(1 2 3 4 5))] [j (in-list/cycle '(1 2))]) j)
                    '(1 2 1 2 1))
      (check-equal? (for/list ([i (in-list '(1 2 3))] [j (in-list/cycle '(1))]) j)
                    '(1 1 1))
      (check-exn exn:fail:contract? (cut in-list/cycle null)))
    
    (test-case "list-swap"
      (check-equal? (list-swap (list 1 2 3 4 5) 1 3) (list 1 4 3 2 5) "non-adjacent items")
      (check-equal? (list-swap (list 1 2 3 4 5) 3 1) (list 1 4 3 2 5) "indices reversed")
      (check-equal? (list-swap (list 1 2 3 4 5) 1 2) (list 1 3 2 4 5) "adjacent items")
      (check-equal? (list-swap (list 1 2 3 4 5) 0 1) (list 2 1 3 4 5) "first two items")
      (check-equal? (list-swap (list 1 2 3 4 5) 3 4) (list 1 2 3 5 4) "last two items")
      (check-exn exn:fail:contract? (cut list-swap (list 1 2 3 4 5) 0 0) "indices the same")
      (check-exn exn:fail:contract? (cut list-swap (list 1 2 3 4 5) -1 0) "indices too low")
      (check-exn exn:fail:contract? (cut list-swap (list 1 2 3 4 5)  4 5)) "indices too high")
    
    (test-case "list-delimit"
      (check-equal? (list-delimit null " ") null)
      (check-equal? (list-delimit '("a") " ") '("a"))
      (check-equal? (list-delimit '("a" "b" "c") " ") '("a" " " "b" " " "c")))
    
    (test-case "list-pad"
      (check-equal? (list-pad (list 1 2 3 4) 7)    (list #f #f #f 1 2 3 4) "default (#f)")
      (check-equal? (list-pad (list 1 2 3 4) 7 #t) (list #t #t #t 1 2 3 4) "#t")
      (check-equal? (list-pad (list 1 2 3 4) 3)    (list 1 2 3 4)          "target-length too small")
      (check-equal? (list-pad (list 1 2 3 4) 4)    (list 1 2 3 4)          "target-length the same"))
    
    (test-case "list-pad-right"
      (check-equal? (list-pad-right (list 1 2 3 4) 7)    (list 1 2 3 4 #f #f #f) "default (#f)")
      (check-equal? (list-pad-right (list 1 2 3 4) 7 #t) (list 1 2 3 4 #t #t #t) "#t")
      (check-equal? (list-pad-right (list 1 2 3 4) 3)    (list 1 2 3 4)          "target-length too small")
      (check-equal? (list-pad-right (list 1 2 3 4) 4)    (list 1 2 3 4)          "target-length the same"))
    
    (test-case "merge-sorted-lists"
      (check-equal? (merge-sorted-lists '(1 3 5 7 9) '(2 4 6 8 10) = <) '(1 2 3 4 5 6 7 8 9 10) "no duplicates")
      (check-equal? (merge-sorted-lists '(1 2 3 4 5) '(3 4 5 6 7)  = <) '(1 2 3 4 5 6 7)        "duplicates")
      (check-equal? (merge-sorted-lists '(1 1 5 7 8 8 9 10) '(1 3 3 15 16 17 18) = <) '(1 3 5 7 8 9 10 15 16 17 18) "general test"))
    
    (test-case "char-iota"
      (check-equal? (char-iota 26)      (string->list "abcdefghijklmnopqrstuvwxyz") "lowercase")
      (check-equal? (char-iota 26 #\A)  (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ") "uppercase")
      (check-equal? (char-iota 5 #\a 2) (string->list "acegi")                      "step 2"))
    
    ; Association lists --------------------------
    
    (test-case "assoc-value"
      (let ([test '((key1 . "Value 1")
                    (key2 . "Value 2")
                    (key3 . "Value 3"))])
        (check equal? (assoc-value 'key1 test) "Value 1")
        (check equal? (assoc-value 'key2 test) "Value 2")
        (check equal? (assoc-value 'key3 test) "Value 3")
        (check-exn exn:fail? (lambda () (assoc-value 'key4 test)))))
    
    (test-case "assoc-value/default"
      (let ([test '((key1 . "Value 1")
                    (key2 . "Value 2")
                    (key3 . "Value 3"))])
        (check equal? (assoc-value/default 'key1 test #f) "Value 1" "check 1")
        (check equal? (assoc-value/default 'key2 test #f) "Value 2" "check 2")
        (check equal? (assoc-value/default 'key3 test #f) "Value 3" "check 3")
        (check equal? (assoc-value/default 'key4 test #f) #f        "check 4")))
    
    (test-case "alist-set"
      (let ([test1 null]
            [test2 null]
            [test3 null])
        (set! test1 (alist-set 'key1 "Value 1" null))
        (check equal? (assoc-value/default 'key1 test1 #f) "Value 1" "check 1a")
        (check equal? (assoc-value/default 'key1 test2 #f) #f        "check 1b")
        (check equal? (assoc-value/default 'key1 test3 #f) #f        "check 1c")
        (check equal? (assoc-value/default 'key2 test1 #f) #f        "check 1d")
        (check equal? (assoc-value/default 'key2 test2 #f) #f        "check 1e")
        (check equal? (assoc-value/default 'key2 test3 #f) #f        "check 1f")
        (check equal? (assoc-value/default 'key3 test1 #f) #f        "check 1g")
        (check equal? (assoc-value/default 'key3 test2 #f) #f        "check 1h")
        (check equal? (assoc-value/default 'key3 test3 #f) #f        "check 1i")
        (set! test2 (alist-set 'key2 "Value 2" test1))
        (check equal? (assoc-value/default 'key1 test1 #f) "Value 1" "check 2a")
        (check equal? (assoc-value/default 'key1 test2 #f) "Value 1" "check 2b")
        (check equal? (assoc-value/default 'key1 test3 #f) #f        "check 2c")
        (check equal? (assoc-value/default 'key2 test1 #f) #f        "check 2d")
        (check equal? (assoc-value/default 'key2 test2 #f) "Value 2" "check 2e")
        (check equal? (assoc-value/default 'key2 test3 #f) #f        "check 2f")
        (check equal? (assoc-value/default 'key3 test1 #f) #f        "check 2g")
        (check equal? (assoc-value/default 'key3 test2 #f) #f        "check 2h")
        (check equal? (assoc-value/default 'key3 test3 #f) #f        "check 2i")
        (set! test3 (alist-set 'key3 "Value 3" test2))
        (check equal? (assoc-value/default 'key1 test1 #f) "Value 1" "check 3a")
        (check equal? (assoc-value/default 'key1 test2 #f) "Value 1" "check 3b")
        (check equal? (assoc-value/default 'key1 test3 #f) "Value 1" "check 3c")
        (check equal? (assoc-value/default 'key2 test1 #f) #f        "check 3d")
        (check equal? (assoc-value/default 'key2 test2 #f) "Value 2" "check 3e")
        (check equal? (assoc-value/default 'key2 test3 #f) "Value 2" "check 3f")
        (check equal? (assoc-value/default 'key3 test1 #f) #f        "check 3g")
        (check equal? (assoc-value/default 'key3 test2 #f) #f        "check 3h")
        (check equal? (assoc-value/default 'key3 test3 #f) "Value 3" "check 3i")))
    
    (test-equal? "alist-map"
      (alist-map string-append '(("a" . "1") ("b" . "2") ("c" . "3")))
      '("a1" "b2" "c3"))
    
    (test-exn "alist-map : non-pair encountered"
      exn:fail:contract?
      (cut alist-map string-append '(("a" . "1") "b" ("c" . "3"))))
    
    (test-case "alist-for-each"
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
    
    (test-exn "alist-for-each : non-pair encountered"
      exn:fail:contract?
      (cut alist-for-each
           (lambda (key value)
             (format "~a:~a~n" key value))
           '(("a" . "1") "b" ("c" . "3"))))
    
    (test-case "alist-merge"
      (check-equal? (alist-merge '((a . 1) (b . 2) (c . 3))
                                 '((b . 4) (c . 5) (d . 6)))
                    '((a . 1) (b . 2) (c . 3) (d . 6))
                    "prefer first (implicit)")
      (check-equal? (alist-merge '((a . 1) (b . 2) (c . 3))
                                 '((b . 4) (c . 5) (d . 6))
                                 'first)
                    '((a . 1) (b . 2) (c . 3) (d . 6))
                    "prefer first (explicit)")
      (check-equal? (alist-merge '((a . 1) (b . 2) (c . 3))
                                 '((b . 4) (c . 5) (d . 6))
                                 'second)
                    '((a . 1) (b . 4) (c . 5) (d . 6))
                    "prefer second"))
    
    ))

; Provide statements -----------------------------

(provide list-tests)
