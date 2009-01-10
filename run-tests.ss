#lang scheme/base

(require (file "all-unlib-tests.ss")
         (file "test-base.ss"))

(print-struct #t)
(error-print-width 1024)

(test/text-ui all-unlib-tests)
