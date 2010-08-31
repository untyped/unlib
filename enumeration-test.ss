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

(define-enum titles
  ([mr   "Mr"]
   [mrs  "Mrs"]
   [miss "Miss"])
  #:equality-test equal?)

; Tests ------------------------------------------

(define/provide-test-suite enumeration-tests
  
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
    (check-equal? (enum-complement vehicle boat plane) '(car))
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
  
  (test-case "title"
    (check-pred enum? titles)
    (check-equal? (enum-values titles) '("Mr" "Mrs" "Miss"))
    (check-equal? (enum-pretty-values titles) '("mr" "mrs" "miss"))
    (check-equal? (enum->string titles) "Mr, Mrs, Miss")
    (check-equal? (enum->pretty-string titles) "mr, mrs, miss")
    (check-equal? (titles mr)   "Mr")
    (check-equal? (titles mrs)  "Mrs")
    (check-equal? (titles miss) "Miss")
    (check-true (enum-value? titles "Mr"))
    (check-true (enum-value? titles "Mrs"))
    (check-true (enum-value? titles "Miss"))
    (check-false (enum-value? titles 'Mr))
    (check-false (enum-value? titles "mrs")))
  
  (test-case "structs"
    (check-true (enum-value? structs (structs one)))
    (check-false (enum-value? structs (make-struct 'a))))
  
  (test-case "underscores"
    (check-equal? (enum-list underscores a b c) '(a b c))
    (check-equal? (enum-complement underscores a b c) null)
    (check-equal? (enum-prettify underscores (underscores a)) "one"))
  
  (test-case "enum-value+false?"
    (check-true  (enum-value? vehicle (vehicle car)))
    (check-false (enum-value? vehicle #f))
    (check-true  (enum-value+false? vehicle (vehicle car)))
    (check-true  (enum-value+false? vehicle #f))
    (check-true  (enum-value+false? titles "Mr"))
    (check-false (enum-value+false? titles 'Mr)))
  
  (test-case "enum->[pretty-]string with separator specified"
    (check-equal? (enum->string vehicle ":") "car:boat:plane")
    (check-equal? (enum->pretty-string vehicle ":") "car:boat:plane")
    (check-equal? (enum->string titles ":") "Mr:Mrs:Miss")
    (check-equal? (enum->pretty-string titles ":") "mr:mrs:miss"))
  
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
    (check-equal? (enum-case structs (make-struct 'a) [(one) 1] [(two) 2] [(three) 3] [else 4]) 4)
    
    (check-equal? (enum-case titles "Mr"   [(mr) 1] [(mrs) 2] [(miss) 3]) 1)
    (check-equal? (enum-case titles "Mrs"  [(mr) 1] [(mrs) 2] [(miss) 3]) 2)
    (check-equal? (enum-case titles "Miss" [(mr) 1] [(mrs) 2] [(miss) 3]) 3)
    (check-exn exn:fail? (cut enum-case titles 'Mr  [(mr) 1] [(mrs) 2] [(miss) 3]))
    (check-exn exn:fail? (cut enum-case titles "mr" [(mr) 1] [(mrs) 2] [(miss) 3])))
  
  (test-case "enum-lambda"
    (let ([proc (enum-lambda option [(a) 10 100] [(b) 20 200] [(c) 30 300])])
      (check-equal? (proc 1) 100)
      (check-equal? (proc 2) 200)
      (check-equal? (proc 3) 300)
      (check-exn exn:fail? (cut proc 'a)))
    (let ([proc (enum-lambda titles [(mr) 10 100] [(mrs) 20 200] [(miss) 30 300])])
      (check-equal? (proc "Mr") 100)
      (check-equal? (proc "Mrs") 200)
      (check-equal? (proc "Miss") 300)
      (check-exn exn:fail? (cut proc 'Mr))))
  
  (test-case "in-enum"
    (check-equal? (for/list ([val (in-enum option)])         val) (list 1 2 3))
    (check-equal? (for/list ([val (in-enum option a c)])     val) (list 1 3))
    (check-equal? (for/list ([val (in-enum titles)])         val) (list "Mr" "Mrs" "Miss"))
    (check-equal? (for/list ([val (in-enum titles mr miss)]) val) (list "Mr" "Miss")))
  
  (test-case "in-enum/pretty"
    (check-equal? (for/list ([val (in-enum/pretty option)])         val) (list "item 1" "item 2" "item 3"))
    (check-equal? (for/list ([val (in-enum/pretty option a c)])     val) (list "item 1" "item 3"))
    (check-equal? (for/list ([val (in-enum/pretty titles)])         val) (list "mr" "mrs" "miss"))
    (check-equal? (for/list ([val (in-enum/pretty titles mr miss)]) val) (list "mr" "miss"))))
