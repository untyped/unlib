#lang scheme/base

(require (file "../test-base.ss")
         (file "check-test.ss"))

(provide all-check-tests)

(define all-check-tests
  (test-suite "check"
    check-tests))
