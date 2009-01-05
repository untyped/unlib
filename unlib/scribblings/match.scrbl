#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "match"]{Match utilities}

@(define-eval match-eval scheme/match (planet untyped/unlib/match))

@defmodule[(planet untyped/unlib/match)]{

Additional patterns for use in @scheme[scheme/match] expressions like @scheme[match] and @scheme[match-lambda].

@defform[(eq? expr pattern ...)]{

Binds values that are @scheme[eq?] to @scheme[expr]. Behaves similarly to the built-in @scheme[?] pattern: if the @scheme[eq?] pattern matches, the other @scheme[pattern]@schemeidfont{s} are also matched against the same value.

@examples[
  #:eval match-eval
  (define x 123)
  (define eq-to-x?
    (match-lambda
      [(eq? x y)
       (format "yes: ~a" y)]
      [_ "no"]))
  (eq-to-x? 123)
  (eq-to-x? 234)]}

@defform[(equal? expr pattern ...)]{

Binds values that are @scheme[equal?] to @scheme[expr]. Behaves similarly to the built-in @scheme[?] pattern: if the @scheme[equal?] pattern matches, the other @scheme[pattern]@schemeidfont{s} are also matched against the same value.

@examples[
  #:eval match-eval
  (define x "123")
  (define equal-to-x?
    (match-lambda
      [(equal? x y)
       (format "yes: ~a" y)]
      [_ "no"]))
  (equal-to-x? "123")
  (equal-to-x? "456")]}

} @;{end defmodule}
