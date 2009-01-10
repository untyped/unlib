#lang scheme/base

(require (file "enum.ss")
         (file "test-base.ss"))

; Helpers ----------------------------------------

(define-enum vehicle
  (car boat plane))

(define-enum prefixed
  ([a "item a"]
   [b 'B "item b"])
  #:prefix prefix:)

; Tests ------------------------------------------

(define enum-tests
  (test-suite "enum.ss"
    
    (test-case "vehicle"
      (check-pred enum? vehicle)
      (check-equal? (enum-values vehicle) '(car boat plane))
      (check-equal? (enum-pretty-values vehicle) '("car" "boat" "plane"))
      (check-equal? (enum->string vehicle) "(U car boat plane)")
      (check-equal? (enum->pretty-string vehicle) "car, boat, plane")
      (check-equal? car 'car)
      (check-equal? boat 'boat)
      (check-equal? plane 'plane)
      (check-true (vehicle? 'car))
      (check-true (vehicle? 'boat))
      (check-true (vehicle? 'plane))
      (check-false (vehicle? 'lemon)))
    
    (test-case "prefixed"
      (check-pred enum? prefixed)
      (check-equal? (enum-values prefixed) '(a B))
      (check-equal? (enum-pretty-values prefixed) '("item a" "item b"))
      (check-equal? (enum->string prefixed) "(U a B)")
      (check-equal? (enum->pretty-string prefixed) "item a, item b")
      (check-equal? prefix:a 'a)
      (check-equal? prefix:b 'B)
      (check-true (prefixed? 'a))
      (check-true (prefixed? 'B))
      (check-false (prefixed? 'b)))))

; Provide statements -----------------------------

(provide enum-tests)