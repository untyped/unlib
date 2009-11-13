#lang scheme/base

(require "enum.ss"
         "test-base.ss")

; Helpers ----------------------------------------

(define-enum vehicle
  (train boat plane))

(define-enum prefixed
  ([a "item a"]
   [b 'B "item b"])
  #:prefix prefix:)

; Tests ------------------------------------------

(define/provide-test-suite enum-tests
  
  (test-case "vehicle"
    (check-pred enum? vehicle)
    (check-equal? (enum-values vehicle) '(train boat plane))
    (check-equal? (enum-pretty-values vehicle) '("train" "boat" "plane"))
    (check-equal? (enum->string vehicle) "train, boat, plane")
    (check-equal? (enum->pretty-string vehicle) "train, boat, plane")
    (check-equal? train 'train)
    (check-equal? boat 'boat)
    (check-equal? plane 'plane)
    (check-true (vehicle? 'train))
    (check-true (vehicle? 'boat))
    (check-true (vehicle? 'plane))
    (check-false (vehicle? 'lemon)))
  
  (test-case "prefixed"
    (check-pred enum? prefixed)
    (check-equal? (enum-values prefixed) '(a B))
    (check-equal? (enum-pretty-values prefixed) '("item a" "item b"))
    (check-equal? (enum->string prefixed) "a, B")
    (check-equal? (enum->pretty-string prefixed) "item a, item b")
    (check-equal? prefix:a 'a)
    (check-equal? prefix:b 'B)
    (check-true (prefixed? 'a))
    (check-true (prefixed? 'B))
    (check-false (prefixed? 'b)))
  
  (test-case "enum->[pretty-]string with separator specified"
    (check-equal? (enum->string vehicle ":") "train:boat:plane")
    (check-equal? (enum->pretty-string vehicle ":") "train:boat:plane")))
