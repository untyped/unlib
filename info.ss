#lang setup/infotab

(define name "unlib")

(define blurb 
  '("A set of utilities developed by Untyped."))

(define release-notes
  '((ul (li "added " (tt "copy-date") " to " (tt "time.ss") ";")
        (li "added " (tt "make-list*") " and " (tt "in-list/cycle") " to " (tt "list.ss") ";")
        (li "added optional key comparison argument to " (tt "alist-set") " in " (tt "list.ss") ";")
        (li "added " (tt "url-remove-params") ", " (tt "url-path-only") " and " (tt "url-local") " to " (tt "url.ss") "."))))

(define primary-file 
  "foo.ss")

(define url "http://svn.untyped.com/unlib/")

(define doc.txt "doc.txt")

(define scribblings '(("doc/unlib.scrbl" (multi-page))))

(define categories '(devtools))

(define required-core-version "4.0")

(define repositories
  '("4.x"))
