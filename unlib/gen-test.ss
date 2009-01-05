(module gen-test mzscheme
  
  (require (lib "cut.ss" "srfi" "26"))
  
  (require (file "gen.ss")
           (file "test-base.ss"))
  
  (provide gen-tests)

  (define gen-tests
    (test-suite
     "gen.ss"

      (test-case
       "all defined values are defined"
       ;; generator-test.ss tests the actual functionality
       ;; of the generator library.  This test simply tests
       ;; all the names that should be defined are defined.
       ;; This check is actually done by the compiler -- all
       ;; we need do is list the names we expect.
       list->generator
       g:end
       g:end?
       g:map
       g:fold
       g:filter
       g:filter-map
       g:remove-duplicates
       g:debug
       g:for-each
       g:collect)
      
    ))

  )
