#lang setup/infotab

(define name "unlib")

(define blurb 
  '("A set of utilities developed by Untyped."))

(define release-notes
  '((p "This update provides a number of new features, including some backwards-incompatible changes to logging to add "
       "compatibility with the logging functions introduced as part of PLT 4.1:")
    (ul (li "added " (tt "generator->hash") " and its shorter alias, " (tt "g:collect/hash") ";")
        (li "added " (tt "string-ellipsify") ";")
        (li "added " (em "enumerations") " as a awy of defining symbolic enumerations with less risk of misspellings;")
        (li "revised " (tt "log.ss") " to make it compatible with the built-in logging in PLT."))))

(define primary-file 
  "foo.ss")

(define url "http://svn.untyped.com/unlib/")

(define scribblings '(("scribblings/unlib.scrbl" (multi-page))))

(define categories '(devtools))

(define required-core-version "4.0.2.5")

(define repositories
  '("4.x"))
