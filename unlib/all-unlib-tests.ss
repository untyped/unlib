(module all-unlib-tests mzscheme
  
  (require (file "check/all-check-tests.ss")
           (file "contract-test.ss")
           (file "date-test.ss")
           (file "debug-test.ss")
           (file "exn-test.ss")
           (file "file-test.ss")
           (file "generator-test.ss")
           (file "gen-test.ss")
           (file "hash-table-test.ss")
           (file "list-test.ss")
           (file "log-test.ss")
           (file "number-test.ss")
           (file "pipeline-test.ss")
           (file "preprocess-test.ss")
           (file "project-test.ss")
           (file "profile-test.ss")
           (file "string-test.ss")
           (file "symbol-test.ss")
           (file "test-base.ss")
           (file "trace-test.ss")
           (file "write-through-cache-test.ss")
           (file "yield-test.ss"))
  
  (provide all-unlib-tests)
  
  (define all-unlib-tests
    (test-suite
     "all-unlib-tests"
     all-check-tests
     contract-tests
     date-tests
     debug-tests
     exn-tests
     file-tests
     generator-tests
     gen-tests
     hash-table-tests
     list-tests
     log-tests
     number-tests
     pipeline-tests
     preprocess-tests
     profile-tests
     project-tests
     string-tests
     symbol-tests
     trace-tests
     write-through-cache-tests
     yield-tests))
  
  )
 
