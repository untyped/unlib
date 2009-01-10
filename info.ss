#lang setup/infotab

(define name "unlib")

(define blurb 
  '("A set of utilities developed by Untyped."))

(define release-notes
  '((p (tt "make-yieldable") " is now implemented using composable continuations.")))

(define primary-file 
  "foo.ss")

(define url "http://svn.untyped.com/unlib/")

(define doc.txt "doc.txt")

(define scribblings '(("doc/unlib.scrbl" (multi-page))))

(define categories '(devtools))

(define required-core-version "4.0")

(define repositories
  '("4.x"))
