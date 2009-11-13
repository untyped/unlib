#lang scheme/base

(require (prefix-in scheme: (only-in scheme/base eq? eqv? equal?))
         srfi/26
         "match.ss"
         "test-base.ss")

; Tests ------------------------------------------

(define/provide-test-suite match-tests
  
  (test-case "eq? and equal? expand to their normal forms outside of a match pattern"
    (check-eq? eq? scheme:eq?)
    (check-eq? equal? scheme:equal?))
  
  (test-case "eq? matches (and does not) in the correct situations"
    (let ([x 1]
          [y "1"])
      (check-equal? (match 1 [(eq? 1) "yes"] [_ "no"]) "yes")
      (check-equal? (match 1 [(eq? x) "yes"] [_ "no"]) "yes")
      (check-equal? (match 2 [(eq? 1) "yes"] [_ "no"]) "no")
      (check-equal? (match 2 [(eq? x) "yes"] [_ "no"]) "no")
      (check-equal? (match "1" [(eq? "1") "yes"] [_ "no"]) "no")
      (check-equal? (match "1" [(eq? y) "yes"] [_ "no"]) "no")
      (check-exn exn:fail? (cut match 2 [(eq? x) "yes"]))))
  
  (test-case "equal? matches (and does not) in the correct situations"
    (let ([x 1]
          [y "1"])
      (check-equal? (match 1 [(equal? 1) "yes"] [_ "no"]) "yes")
      (check-equal? (match 1 [(equal? x) "yes"] [_ "no"]) "yes")
      (check-equal? (match 2 [(equal? 1) "yes"] [_ "no"]) "no")
      (check-equal? (match 2 [(equal? x) "yes"] [_ "no"]) "no")
      (check-equal? (match "1" [(equal? "1") "yes"] [_ "no"]) "yes")
      (check-equal? (match "1" [(equal? y) "yes"] [_ "no"]) "yes")
      (check-exn exn:fail? (cut match 2 [(equal? x) "yes"]))))
  
  (test-case "eq? binds correctly"
    (let ([num1 123]
          [num2 123])
      (match num1
        ; Match num1 against num2 and (the confusing bit) rebind num2 inside the match body:
        [(eq? num2 num2)
         ; Mutate the inner version of num2:
         (set! num2 1000)])
      ; We should be back to the outer version now:
      (check-eq? num2 123)))
  
  (test-case "equal? binds correctly"
    (let ([str1 "123"]
          [str2 "123"]
          [str3 #f])
      (check-false (eq? str1 str2))
      (check-true  (equal? str1 str2))
      (match str1
        ; Match str1 against str2 and (the confusing bit) rebind str2 inside the match body:
        [(equal? str2 str2)
         ; The inner str1 should be eq? to str2:
         (check-true (eq? str1 str2))
         (check-true (equal? str1 str2))
         ; Store the inner str1 in str3:
         (set! str3 str2)])
      ; str3 should now be a copy of str1:
      (check-false (eq? str1 str2))
      (check-true  (equal? str1 str2))
      (check-true  (eq? str1 str3))
      (check-true  (equal? str1 str3))
      (check-false (eq? str2 str3))
      (check-true  (equal? str2 str3)))))

; Provide statements -----------------------------

(provide match-tests)
