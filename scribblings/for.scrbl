#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "for"]{@scheme[for] variants}

@(define-eval for-eval (planet untyped/unlib/for))

@defmodule[(planet untyped/unlib/for)]{

@defform[(for/fold/reverse ([accum-id accum-expr] ...) ([sequence-id sequence-expr] ...) expr ...)]{
Like @scheme[for/fold], but calls @scheme[reverse] on each @scheme[accum-id] after iteration is complete.

@examples[
  #:eval for-eval
  (for/fold/reverse
   ([even null]
    [odd  null])
   ([i (in-range 1 10)])
   (if (even? i)
       (values (cons i even) odd)
       (values even (cons i odd))))]}

@defform[(for/fold1 ([accum-id accum-expr] ...) ([sequence-id sequence-expr] ...) expr ...)]{
Like @scheme[for/fold], but returns only the value of the first @scheme[accum-id].

@examples[
  #:eval for-eval
  (for/fold1
   ([even null]
    [odd  null])
   ([i (in-range 1 10)])
   (if (even? i)
       (values (cons i even) odd)
       (values even (cons i odd))))]}

@defform[(for/fold1/reverse ([accum-id accum-expr] ...) ([sequence-id sequence-expr] ...) expr ...)]{
Like @scheme[for/fold1], but returns the @scheme[reverse] of the first @scheme[accum-id].

@examples[
  #:eval for-eval
  (for/fold1
   ([even null]
    [odd  null])
   ([i (in-range 1 10)])
   (if (even? i)
       (values (cons i even) odd)
       (values even (cons i odd))))]}

@defform[(for/filter ([sequence-id sequence-expr] ...) expr ...)]{
Like @scheme[for/list], but only accumulates non-@scheme[#f] return values.

@examples[
  #:eval for-eval
  (for/filter ([i (in-range 1 10)])
    (and (even? i) i))]}

@defform[(for/append ([sequence-id sequence-expr] ...) expr ...)]{
Like @scheme[for/list], but @scheme[append]@schemeidfont{s} the results into a list.

@examples[
  #:eval for-eval
  (for/append ([i (in-range 1 5)])
    (list i (* i 2)))]}

} @;{end defmodule}
