#lang setup/infotab

(define name    "Unlib")
(define version "3.x")
(define url     "http://svn.untyped.com/unlib/")

(define blurb
  '("A set of utilities developed by Untyped."))

(define release-notes
  '((p "Changes and additions:")
    (ul (li "added " (tt "for.ss") " containing several useful " (tt "for") " variants;")
        (li "added " (tt "enumeration.ss") ", containing a new version of " (tt "enum.ss") ";")
        (li "added " (tt "list-diff") " to " (tt "list.ss") ";")
        (li "added optional " (tt "#:short?") " argument to " (tt "time->ago-string") " and " (tt "seconds->ago-string") "."))))

(define scribblings
  '(("scribblings/unlib.scrbl" (multi-page))))

(define primary-file 
  "foo.ss")

(define categories            '(devtools))
(define required-core-version "4.0.2.5")
(define repositories          '("4.x"))
