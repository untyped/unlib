#lang scheme/base

(require "test-base.ss")

(require (for-syntax scheme/base
                     "syntax.ss"))

(define syntax-tests
  (test-suite "syntax.ss"
    
    (test-case "dotted-identifier?"
      (let-syntax ([dotted?
                    (lambda (stx)
                      (datum->syntax
                       stx 
                       (syntax-case stx ()
                         [(_ id)         (dotted-identifier? #'id)]
                         [(_ id min)     (dotted-identifier?
                                          #'id
                                          (syntax->datum #'min))]
                         [(_ id min max) (dotted-identifier?
                                          #'id
                                          (syntax->datum #'min)
                                          (syntax->datum #'max))])))])
        (check-false (dotted? a))
        (check-true  (dotted? a 1))
        (check-true  (dotted? a 1 1))
        
        (check-true  (dotted? a.b))
        (check-true  (dotted? a.b 2))
        (check-true  (dotted? a.b 2 3))
        (check-true  (dotted? a.b 1 2))
        (check-false (dotted? a.b 1 1))
        (check-false (dotted? a.b 3 3))
        
        (check-true  (dotted? .a.b))
        (check-true  (dotted? a..b))
        (check-true  (dotted? a.b.))
        (check-true  (dotted? a.b.c))))
    
    (test-case "simple-dotted-identifier?"
      (let-syntax ([dotted?
                    (lambda (stx)
                      (datum->syntax
                       stx 
                       (syntax-case stx ()
                         [(_ id)         (simple-dotted-identifier? #'id)]
                         [(_ id min)     (simple-dotted-identifier?
                                          #'id
                                          (syntax->datum #'min))]
                         [(_ id min max) (simple-dotted-identifier?
                                          #'id
                                          (syntax->datum #'min)
                                          (syntax->datum #'max))])))])
        (check-false (dotted? a))
        (check-true  (dotted? a.b))
        (check-false (dotted? .a.b))
        (check-false (dotted? a..b))
        (check-false (dotted? a.b.))
        (check-true  (dotted? a.b.c))
        (check-false (dotted? a.b.c 2 2))))
    
    (test-case "dotted-identifier-count"
      (let-syntax ([count
                    (lambda (stx)
                      (datum->syntax
                       stx 
                       (syntax-case stx ()
                         [(_ id) (dotted-identifier-count #'id)])))])
        (check-equal? (count a)     1)
        (check-equal? (count a.)    2)
        (check-equal? (count .a)    2)
        (check-equal? (count a.b)   2)
        (check-equal? (count a..b)  3)
        (check-equal? (count a.b.c) 3)))
    
    (test-case "dotted-identifier-split"
      (let-syntax ([split
                    (lambda (stx)
                      (syntax-case stx ()
                         [(_ id) #`(list #,@(dotted-identifier-split #'id))]))])
        (let ([a 1] [b 2] [c 3])
          (check-equal? (split a)     (list 1))
          (check-equal? (split a.b)   (list 1 2))
          (check-equal? (split a.b.c) (list 1 2 3)))))))

; Provide statements -----------------------------

(provide syntax-tests)