(module info (lib "infotab.ss" "setup")
  (define name "unlib")

  (define compile-omit-files
    '("run-tests.ss" "jsmake.ss"))

  (define blurb 
    '("A set of utilities developed by Untyped."))

  (define release-notes
    '((ul (li "renamed " (tt "generator-fold") " to " (tt "generator-fold-map") ";")
          (li "renamed " (tt "generator-accumulate") " to " (tt "generator-fold") ";")
          (li "renamed " (tt "g:fold") " to " (tt "g:fold-map") ";")
          (li "renamed " (tt "g:accumulate") " to " (tt "g:fold") ";")
          (li "finalised and documented " (tt "generator-project") " and " (tt "g:project") "."))))
  
  (define url "http://svn.untyped.com/unlib/")

  (define doc.txt "doc.txt")
  (define categories '(devtools))

  )
