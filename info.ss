#lang setup/infotab

(define name    "Unlib")
(define version "3.x")
(define url     "http://svn.untyped.com/unlib/")

(define blurb
  '("A set of utilities developed by Untyped."))

(define release-notes
  '((p "Changes and additions:")
    (ul (li "added " (tt "dotted-identifier?") ", " (tt "simple-dotted-identifier?") ", "
            (tt "dotted-identifier-count") " and " (tt "dotted-identifier-split") ".")
        (li "added optional " (tt "#:format") " argument to control the text formatting in "
            (tt "time->ago-string") " and " (tt "seconds->ago-string") ";")
        (li "added " (tt "file-pretty-size") " and " (tt "prettify-file-size") ";")
        (li "added " (tt "crc.ss") " and " (tt "crc32") "."))))

(define scribblings
  '(("scribblings/unlib.scrbl" (multi-page))))

(define primary-file 
  "foo.ss")

(define categories            '(devtools))
(define required-core-version "4.0.2.5")
(define repositories          '("4.x"))
