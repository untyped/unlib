#lang scheme/base

(require "enumeration.ss"
         "test-base.ss")

; Helpers ----------------------------------------

(define-enum vehicle
  (car boat plane))

(define-enum number
  ([one   1 "one"]
   [two   2 "two"]
   [three 3 "three"]))

(define-enum option
  ([a 1 "item 1"]
   [b 2 "item 2"]
   [c 3 "item 3"]))

(define-struct struct (val) #:transparent)

(define-enum structs
  ([one   (make-struct 'a) "one"]
   [two   (make-struct 'b) "two"]
   [three (make-struct 'c) "three"]))

(define-enum underscores
  ([a _ "one"]
   [b _ "two"]
   [c _ "three"]))

; Tests ------------------------------------------

(define enumeration-tests
  (test-suite "enumeration.ss"
    
    (test-case "vehicle"
      (check-pred enum? vehicle)
      (check-equal? (enum-values vehicle) '(car boat plane))
      (check-equal? (enum-pretty-values vehicle) '("car" "boat" "plane"))
      (check-equal? (enum->string vehicle) "car, boat, plane")
      (check-equal? (enum->pretty-string vehicle) "car, boat, plane")
      (check-equal? (vehicle car) 'car)
      (check-equal? (vehicle boat) 'boat)
      (check-equal? (vehicle plane) 'plane)
      (check-equal? (enum-list vehicle boat plane) '(boat plane))
      (check-equal? (enum-compliment vehicle boat plane) '(car))
      (check-true (enum-value? vehicle 'car))
      (check-true (enum-value? vehicle 'boat))
      (check-true (enum-value? vehicle 'plane))
      (check-false (enum-value? vehicle 'lemon)))
        
    (test-case "option"
      (check-pred enum? option)
      (check-equal? (enum-values option) '(1 2 3))
      (check-equal? (enum-pretty-values option) '("item 1" "item 2" "item 3"))
      (check-equal? (enum->string option) "1, 2, 3")
      (check-equal? (enum->pretty-string option) "item 1, item 2, item 3")
      (check-equal? (option a) 1)
      (check-equal? (option b) 2)
      (check-equal? (option c) 3)
      (check-true (enum-value? option 1))
      (check-true (enum-value? option 2))
      (check-true (enum-value? option 3))
      (check-false (enum-value? option 'a))
      (check-false (enum-value? option 'b))
      (check-false (enum-value? option 'c)))
    
    (test-case "structs"
      (check-true (enum-value? structs (structs one)))
      (check-false (enum-value? structs (make-struct 'a))))
    
    (test-case "underscores"
      (check-equal? (enum-list underscores a b c) '(a b c))
      (check-equal? (enum-compliment underscores a b c) null)
      (check-equal? (enum-prettify underscores (underscores a)) "one"))
    
    (test-case "enum-value+false?"
      (check-true  (enum-value? vehicle (vehicle car)))
      (check-false (enum-value? vehicle #f))
      (check-true  (enum-value+false? vehicle (vehicle car)))
      (check-true  (enum-value+false? vehicle #f)))
    
    (test-case "enum->[pretty-]string with separator specified"
      (check-equal? (enum->string vehicle ":") "car:boat:plane")
      (check-equal? (enum->pretty-string vehicle ":") "car:boat:plane"))
    
    (test-case "enum-case"
      (check-equal? (enum-case option 1 [(a) 10 100] [(b) 20 200] [(c) 30 300]) 100)
      (check-equal? (enum-case option 2 [(a) 10 100] [(b) 20 200] [(c) 30 300]) 200)
      (check-equal? (enum-case option 3 [(a) 10 100] [(b) 20 200] [(c) 30 300]) 300)
      (check-exn exn:fail? (cut enum-case option 'a [(a) 100] [(b) 200] [(c) 300]))
      
      (check-equal? (enum-case option 1 [(a) 10 100] [(b) 20 200] [else 30 300]) 100)
      (check-equal? (enum-case option 2 [(a) 10 100] [(b) 20 200] [else 30 300]) 200)
      (check-equal? (enum-case option 3 [(a) 10 100] [(b) 20 200] [else 30 300]) 300)
      (check-equal? (enum-case option 'a [(a) 100] [(b) 200] [else 300]) 300)
      
      (check-equal? (enum-case option 1 [(a) 10 100] [(b c) 20 200]) 100)
      (check-equal? (enum-case option 2 [(a) 10 100] [(b c) 20 200]) 200)
      (check-equal? (enum-case option 3 [(a) 10 100] [(b c) 20 200]) 200)
      (check-exn exn:fail? (cut enum-case option 'a [(a) 100] [(b c) 200]))
      
      (check-equal? (enum-case structs (structs one) [(one) 1] [(two) 2] [(three) 3] [else 4]) 1)
      (check-equal? (enum-case structs (make-struct 'a) [(one) 1] [(two) 2] [(three) 3] [else 4]) 4))
      
    (test-case "in-enum"
      (check-equal? (for/list ([val (in-enum option)])     val) (list 1 2 3))
      (check-equal? (for/list ([val (in-enum option a c)]) val) (list 1 3)))
        
    (test-case "in-enum/pretty"
      (check-equal? (for/list ([val (in-enum/pretty option)]) val) (list "item 1" "item 2" "item 3"))
      (check-equal? (for/list ([val (in-enum/pretty option a c)]) val) (list "item 1" "item 3")))))

; Provide statements -----------------------------

(provide enumeration-tests)
