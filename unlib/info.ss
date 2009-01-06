#lang setup/infotab

(define name    "Unlib")
(define version "3.x")
(define url     "http://svn.untyped.com/unlib/")

(define blurb
  '("A set of utilities developed by Untyped."))

(define release-notes
  '((p "Changes and additions:")
    (ul (li "added " (tt "keyword-apply*") " macro in " (tt "keyword.ss") "."))))

(define scribblings
  '(("scribblings/unlib.scrbl" (multi-page))))

(define primary-file 
  "foo.ss")

(define categories            '(devtools))
(define required-core-version "4.0.2.5")
(define repositories          '("4.x"))

; (define licence-file     "COPYING")
; (define licence-template "COPYING-template")
; (define copyright-holder "Untyped Ltd")
; (define copyright-year   "2008")

; (define test-file        "all-tests.ss")
; (define test-suite       'all-tests)
