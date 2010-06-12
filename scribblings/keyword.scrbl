#lang scribble/doc

@(require "base.ss")

@title[#:tag "keyword"]{Keyword utilities}

@(define-eval keyword-eval (planet untyped/unlib/keyword))

@defmodule[(planet untyped/unlib/keyword)]{

Utilities for use with keywords and keyword procedures.

@defproc[(keyword-apply* [proc procedure?] [arg any] ... [rest list?]) any]{

A more humane version of @scheme[keyword-apply], where the arguments are specified in a similar order to a regular procedure call. Keywords should be quoted.

@examples[
  #:eval keyword-eval
  (define (test #:a [a 1] b . rest)
    (list a b rest))
  (keyword-apply* test null)
  (keyword-apply* test 2 3 4 5 null)
  (keyword-apply* test (list 2 3 4 5))
  (keyword-apply* test '#:a 123 456 null)
  (keyword-apply* test 456 (list '#:a 123))]}

} @;{end defmodule}
