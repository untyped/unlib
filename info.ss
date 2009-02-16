#lang setup/infotab

(define name    "Unlib")
(define version "3.x")
(define url     "http://svn.untyped.com/unlib/")

(define blurb
  '("A set of utilities developed by Untyped."))

(define release-notes
  '((p "Changes and additions:")
    (ul (li "simplified the syntaxes produced by " (tt "define-library-aliases") ";")
        (li "added " (tt "list-ref?") " to " (tt "list.ss") ";")
        (li "added slash-style aliases for various " (tt "debug") " forms (" (tt "let/debug") " and so on; "
            "the old hyphen-style aliases will be removed in a future version of Unlib);")
        (li "documented forthcoming backwards-incompatible changes in Unlib 4."))))

(define scribblings
  '(("scribblings/unlib.scrbl" (multi-page))))

(define primary-file 
  "foo.ss")

(define categories            '(devtools))
(define required-core-version "4.0.2.5")
(define repositories          '("4.x"))
