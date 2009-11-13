#lang scheme/base

(require (except-in srfi/1 any)
         "project.ss"
         "test-base.ss")

; Helpers ------------------------------

; integer -> (listof (listof (U #t #f)))
(define (binary-sequences len)
  (if (= len 1)
      (list (list #f) (list #t))
      (let ([sub-sequences (binary-sequences (sub1 len))])
        (append (map (cut cons #f <>) sub-sequences)
                (map (cut cons #t <>) sub-sequences)))))

; Tests --------------------------------

(define/provide-test-suite project-tests
  
  (test-case "project+fold calls proc the correct number of times"
    (let ([num-calls
           (lambda (data)
             (let ([counter 0])
               (project+fold (lambda (data accum)
                               (set! counter (add1 counter))
                               (add1 accum))
                             0
                             data
                             (list #t #f #f))
               counter))])
      (check-equal? (num-calls null) 0)
      (check-equal? (num-calls (list (list 1 2 3))) 1)
      (check-equal? (num-calls (list (list 1 2 3)
                                     (list 1 2 3))) 1)
      (check-equal? (num-calls (list (list 1 2 3)
                                     (list 2 2 3))) 2)
      (check-equal? (num-calls (list (list 1 2 3)
                                     (list 1 2 3)
                                     (list 2 2 3))) 2)
      (check-equal? (num-calls (list (list 1 2 3)
                                     (list 2 2 3)
                                     (list 2 2 3))) 2)
      (check-equal? (num-calls (list (list 1 2 3)
                                     (list 2 2 3)
                                     (list 1 2 3))) 3)))
  
  (test-case "project+fold accumulates data correctly"
    (let ([cons+accum
           (lambda (data)
             (project+fold cons
                           null
                           data
                           (list #t #f #f)))])
      (check-equal? (cons+accum null) null)
      (check-equal? (cons+accum (list (list 1 2 3)))
                    (list (list 1 (list (list 2 3)))))
      (check-equal? (cons+accum (list (list 1 2 3)
                                      (list 1 2 3)))
                    (list (list 1 (list (list 2 3)
                                        (list 2 3)))))
      (check-equal? (cons+accum (list (list 1 2 3)
                                      (list 4 5 6)))
                    (list (list 4 (list (list 5 6)))
                          (list 1 (list (list 2 3)))))
      (check-equal? (cons+accum (list (list 1 2 3)
                                      (list 1 4 5)
                                      (list 2 6 7)))
                    (list (list 2 (list (list 6 7)))
                          (list 1 (list (list 2 3)
                                        (list 4 5)))))
      (check-equal? (cons+accum (list (list 1 2 3)
                                      (list 4 5 6)
                                      (list 4 7 8)))
                    (list (list 4 (list (list 5 6)
                                        (list 7 8)))
                          (list 1 (list (list 2 3)))))
      (check-equal? (cons+accum (list (list 1 2 3)
                                      (list 2 4 5)
                                      (list 1 6 7)))
                    (list (list 1 (list (list 6 7)))
                          (list 2 (list (list 4 5)))
                          (list 1 (list (list 2 3)))))))
  
  (test-case "project accumulates data correctly"
    (let ([cons+accum
           (lambda (data)
             (project data (list #t #f #f)))])
      (check-equal? (cons+accum null) null)
      (check-equal? (cons+accum (list (list 1 2 3)))
                    (list (list 1 (list (list 2 3)))))
      (check-equal? (cons+accum (list (list 1 2 3)
                                      (list 1 2 3)))
                    (list (list 1 (list (list 2 3)
                                        (list 2 3)))))
      (check-equal? (cons+accum (list (list 1 2 3)
                                      (list 4 5 6)))
                    (list (list 1 (list (list 2 3)))
                          (list 4 (list (list 5 6)))))
      (check-equal? (cons+accum (list (list 1 2 3)
                                      (list 1 4 5)
                                      (list 2 6 7)))
                    (list (list 1 (list (list 2 3)
                                        (list 4 5)))
                          (list 2 (list (list 6 7)))))
      (check-equal? (cons+accum (list (list 1 2 3)
                                      (list 4 5 6)
                                      (list 4 7 8)))
                    (list (list 1 (list (list 2 3)))
                          (list 4 (list (list 5 6)
                                        (list 7 8)))))
      (check-equal? (cons+accum (list (list 1 2 3)
                                      (list 2 4 5)
                                      (list 1 6 7)))
                    (list (list 1 (list (list 2 3)))
                          (list 2 (list (list 4 5)))
                          (list 1 (list (list 6 7)))))))
  
  (test-case "project+map works as expected"
    (let ([do-map
           (lambda (data)
             (project+map (lambda (key rest)
                            ; Return (cons key (sum-of-all nonkey))
                            (cons key (fold + 0 (map (cut fold + 0 <>) rest))))
                          data
                          (list #t #f #f)))])
      (check-equal? (do-map null) null)
      (check-equal? (do-map (list (list 1 2 3)))
                    (list (cons 1 (+ 2 3))))
      (check-equal? (do-map (list (list 1 2 3)
                                  (list 1 2 3)))
                    (list (cons 1 (+ 2 3 2 3))))
      (check-equal? (do-map (list (list 1 2 3)
                                  (list 4 5 6)))
                    (list (cons 1 (+ 2 3))
                          (cons 4 (+ 5 6))))
      (check-equal? (do-map (list (list 1 2 3)
                                  (list 1 4 5)
                                  (list 2 6 7)))
                    (list (cons 1 (+ 2 3 4 5))
                          (cons 2 (+ 6 7))))
      (check-equal? (do-map (list (list 1 2 3)
                                  (list 4 5 6)
                                  (list 4 7 8)))
                    (list (cons 1 (+ 2 3))
                          (cons 4 (+ 5 6 7 8))))
      (check-equal? (do-map (list (list 1 2 3)
                                  (list 2 4 5)
                                  (list 1 6 7)))
                    (list (cons 1 (+ 2 3))
                          (cons 2 (+ 4 5))
                          (cons 1 (+ 6 7))))))
  
  (test-case "project+for-each works as expected"
    (let* ([counter 0]
           [do-map
            (lambda (data)
              (set! counter 0)
              (project+map (lambda (key rest)
                             ; Return (cons key (sum-of-all nonkey))
                             (set! counter (+ counter key (fold + 0 (map (cut fold + 0 <>) rest)))))
                           data
                           (list #t #f #f))
              counter)])
      (check-equal? (do-map null) 0)
      (check-equal? (do-map (list (list 1 2 3)))
                    (+ 1
                       2 3))
      (check-equal? (do-map (list (list 1 2 3)
                                  (list 1 2 3)))
                    (+ 1
                       2 3 
                       2 3))
      (check-equal? (do-map (list (list 1 2 3)
                                  (list 4 5 6)))
                    (+ 1
                       2 3
                       4
                       5 6))
      (check-equal? (do-map (list (list 1 2 3)
                                  (list 1 4 5)
                                  (list 2 6 7)))
                    (+ 1
                       2 3
                       4 5
                       2
                       6 7))
      (check-equal? (do-map (list (list 1 2 3)
                                  (list 4 5 6)
                                  (list 4 7 8)))
                    (+ 1
                       2 3
                       4
                       5 6
                       7 8))
      (check-equal? (do-map (list (list 1 2 3)
                                  (list 2 4 5)
                                  (list 1 6 7)))
                    (+ 1
                       2 3
                       2
                       4 5
                       1
                       6 7)))))
