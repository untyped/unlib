#lang setup/infotab

(define name "unlib")

(define blurb 
  '("A set of utilities developed by Untyped."))

(define release-notes
  '((p "Changes and additions:")
    (ul (li "added "
            (tt "define-values-debug") ", " 
            (tt "let-values-debug") ", "
            (tt "let*-values-debug") ", "
            (tt "letrec-values-debug") ", "
            (tt "debug-in") " and "
            (tt "debug-out") ";")
        (li "added " (tt "unzip-values") " procedure in " (tt "list.ss") ";")
        (li "added " (tt "path-contains?") " procedure in " (tt "file.ss") ";")
        (li "added " (tt "match.ss") " containing " (tt "eq?") " and " (tt "equal?") " patterns for use with the " (tt "scheme/match") " library;")
        (li "added optional " (em "separator") " argument to " (tt "enum->string") " and " (tt "enum->pretty-string") ";")
        (li "removed " (tt "stage") ", " (tt "find-stage") " and " (tt "define-stage") ";")
        (li "unit tests ported to " (em "SchemeUnit 3") "."))))

(define primary-file 
  "foo.ss")

(define url "http://svn.untyped.com/unlib/")

(define scribblings '(("scribblings/unlib.scrbl" (multi-page))))

(define categories '(devtools))

(define required-core-version "4.0.2.5")

(define repositories
  '("4.x"))
