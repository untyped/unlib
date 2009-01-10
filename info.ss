(module info (lib "infotab.ss" "setup")
  (define name "unlib")

  (define compile-omit-files
    '("run-tests.ss" "jsmake.ss"))

  (define blurb 
    '("A set of utilities developed by Untyped."))

  (define release-notes
    '("Added " (tt "gen->") " to gen.ss."))
  
  (define url "http://svn.untyped.com/unlib/")

  (define version "2.1")

  (define doc.txt "doc.txt")
  (define categories '(devtools))

  )
