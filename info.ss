#lang setup/infotab

(define name    "Unlib")
(define version "3.x")
(define url     "http://svn.untyped.com/unlib/")

(define blurb
  '("A set of utilities developed by Untyped."))

(define release-notes
  '((p "Changes and additions:")
    (ul (li "fixed infinite loop when parsing some badly formed " (tt "define-enum") " statements;")
        (li "added " (tt "in-enum") " and " (tt "in-enum/pretty") " forms to " (tt "enumeration.ss") ";")
        (li "added " (tt "enum-value/c") " to " (tt "enumeration.ss") " and " (tt "enum.ss") ";")
        (li "added " (tt "generator-append") " and " (tt "g:append") "."))))

(define scribblings
  '(("scribblings/unlib.scrbl" (multi-page))))

(define primary-file 
  "foo.ss")

(define categories            '(devtools))
(define required-core-version "4.0.2.5")
(define repositories          '("4.x"))
