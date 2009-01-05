#lang scheme/base

(require (file "gen.ss")
         (file "test-base.ss"))

; Tests ------------------------------------------

(define gen-tests
  (test-suite "gen.ss"
    
    (test-case "all defined values are defined"
      ; generator-test.ss tests the actual functionality of the generator library.
      ; This test simply tests all the names that should be defined are defined.
      ; This check is actually done by the compiler -- all the test does is list the names it expects.
      g:list
      g:range
      g:end
      g:end?
      g:map
      g:fold
      g:filter
      g:filter-map
      g:remove-duplicates
      g:debug
      g:for-each
      g:collect
      g:collect/hash)))

; Provide statements -----------------------------

(provide gen-tests)
