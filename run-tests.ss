#lang scheme/base

(require "all-unlib-tests.ss"
         "test-base.ss")

(print-struct #t)
(error-print-width 1024)

(run-tests all-unlib-tests)
