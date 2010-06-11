#lang setup/infotab

(define name    "Unlib")
(define version "3.x")
(define url     "http://svn.untyped.com/unlib/")

(define blurb
  '("A set of utilities developed by Untyped."))

(define release-notes
  '((p "Changes and additions:")
    (ul (li "added " (tt "enum-lambda") " to " (tt "enumeration.ss") ";")
        (li "added " (em "\"let loop\"") " form to " (tt "let/debug") ";")
        (li "added " (tt "date.ss") ", which wraps " (tt "bzlib/date.plt") " and " (tt "bzlib/date-tz.plt") 
            " to provide a set of time-zone- / daylight-saving-aware date functions."))))

(define scribblings
  '(("scribblings/unlib.scrbl" (multi-page))))

(define primary-file 
  "foo.ss")

(define categories            '(devtools))
(define required-core-version "4.0.2.5")
(define repositories          '("4.x"))

(define compile-omit-paths
  '("autoplanet.ss"
    "build.ss"))