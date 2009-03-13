#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "syntax"]{Syntax utilities}

@(define-eval syntax-eval (planet untyped/unlib/syntax))

@defmodule[(planet untyped/unlib/syntax)]{
Utilities for creating macros and working with syntax.

@defproc[(symbolic-identifier=? [stx1 syntax?] [stx2 syntax?]) boolean?]{
Compares two identifiers based on their symbolic representation.}

@defproc[(make-id [stx (U syntax? #f)] [arg (U syntax? string? symbol? number?)] ...) syntax?]{
Creates an identifier by appending @scheme[arg]@schemeidfont{s}. Equivalent to:

@schemeblock[(datum->syntax stx (string->symbol (apply string-append (map arg->string args))))]

where @scheme[arg->string] converts an argument to a string.}

@defproc[(syntax-location-string [stx syntax?]) string?]{
Returns a string describing the source location of @scheme[stx] (for example @scheme["myfile.ss:123:45"]).}

@defform/subs[(begin-for-syntax/any-order definition ...)
              ([definition (define (id arg ...) expr ...)
                           (define id expr)])]{
Like @scheme[begin-for-syntax] except that definitions can refer to previous definitions in the manner of a @scheme[letrec] statement. Only definitions are allowed within the body of the form.}

@defproc[(dotted-identifier?
          [stx       syntax?]
          [min-parts (U natural? #f) 2]
          [max-parts (U natural? #f) #f])
         boolean?]{
Returns @scheme[#f] if @scheme[stx] represents an identifier comprised of one or more @italic{parts} joined by dots. Parts can be zero-length.

The @scheme[min-parts] and @scheme[max-parts] arguments can be used limit the number of allowed parts (both limits are inclusive).

@examples[
  #:eval syntax-eval
  (dotted-identifier? #'a)
  (dotted-identifier? #'.a)
  (dotted-identifier? #'a.b)
  (dotted-identifier? #'a.)
  (dotted-identifier? #'a.b.c 3 3)
  (dotted-identifier? #'a 1)]}

@defproc[(simple-dotted-identifier?
          [stx       syntax?]
          [min-parts (U natural? #f) 2]
          [max-parts (U natural? #f) #f])
         boolean?]{
Like @scheme[dotted-identifier?] except all parts must be a minimum length of 1.

@examples[
  #:eval syntax-eval
  (simple-dotted-identifier? #'a)
  (simple-dotted-identifier? #'a.b)
  (simple-dotted-identifier? #'.a)
  (simple-dotted-identifier? #'a.)]}

@defproc[(dotted-identifier-count [dotted-id-stx identifier?]) natural?]{
Returns the number of parts in @scheme[dotted-id-stx].

@examples[
  #:eval syntax-eval
  (dotted-identifier-count #'a)
  (dotted-identifier-count #'a.b)
  (dotted-identifier-count #'a.b.c)
  (dotted-identifier-count #'.a)]}

@defproc[(dotted-identifier-split [dotted-id-stx identifier?]) (listof identifier?)]{
Splits @scheme[dotted-id-stx] into its constituent parts.

@examples[
  #:eval syntax-eval
  (map syntax->datum (dotted-identifier-split #'a))
  (map syntax->datum (dotted-identifier-split #'a.b))
  (map syntax->datum (dotted-identifier-split #'a.b.c))
  (map syntax->datum (dotted-identifier-split #'.a))]}

} @;{end defmodule}