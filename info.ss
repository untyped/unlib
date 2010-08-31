#lang setup/infotab

(define name    "Unlib")
(define version "3.x")
(define url     "http://svn.untyped.com/unlib/")

(define blurb
  '("A set of utilities developed by Untyped."))

(define release-notes
  '((p "Changes and additions:")
    (ul (li "added optional " (tt "#:equality-test") " argument to " (tt "define-enum") " form in " 
            (tt "enumeration.ss") ", paving the way for string enumerations;")
        (li "added " (tt "round-to") " to " (tt "number.ss") ";")
        (li "added " (tt "natural->hex-string") " and " (tt "hex-string->natural") " to " (tt "number.ss") " and " (tt "string.ss") ";")
        (li "documentation fixes (thanks to Eric Hanchrow for spotting these)."))))

(define scribblings
  '(("scribblings/unlib.scrbl" (multi-page))))

(define primary-file 
  "foo.ss")

(define categories            '(devtools))
(define required-core-version "4.0.2.5")
(define repositories          '("4.x"))

(define compile-omit-paths
  '("autoplanet.ss"
    "build.ss"
    "planet"
    "planetdev"))
