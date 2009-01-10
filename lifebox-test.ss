#lang scheme/base

(require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
(require "lifebox.ss")
  
(provide lifebox-tests)

(define lifebox-tests
  (test-suite
   "All tests for lifebox"

   (test-case
    "lifebox-expired?"
    (check-pred lifebox-expired? (make-lifebox 0 'hi))
    (check-false (lifebox-expired? (make-lifebox (+ (current-seconds) 100) 'hi)))
    (check-true  (lifebox-expired? (make-lifebox 100 'hi) 100))
    (check-false (lifebox-expired? (make-lifebox 100 'hi) 99)))
   ))
