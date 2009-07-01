#lang scheme/base

(require "enumeration.ss"
         "test-base.ss")

; Helpers ----------------------------------------

(define-enum vehicle
  (car boat plane))

(define-enum option
  ([a 1 "item 1"]
   [b 2 "item 2"]
   [c 3 "item 3"]))

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
      (check-exn exn:fail? (cut enum-case option 'a [(a) 100] [(b) 200] [else 300]))
      (check-equal? (enum-case option 1 [(a) 10 100] [(b c) 20 200]) 100)
      (check-equal? (enum-case option 2 [(a) 10 100] [(b c) 20 200]) 200)
      (check-equal? (enum-case option 3 [(a) 10 100] [(b c) 20 200]) 200)
      (check-exn exn:fail? (cut enum-case option 'a [(a) 100] [(b c) 200])))))

; Provide statements -----------------------------

(provide enumeration-tests)
