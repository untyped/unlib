#lang scheme/base

(require "../test-base.ss"
         "check-test.ss")

(provide all-check-tests)

(define all-check-tests
  (test-suite "check"
    check-tests))
