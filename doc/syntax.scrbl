#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "syntax"]{Syntax utilities}

@defmodule[(planet untyped/unlib/syntax) #:use-sources ((file "syntax.ss"))]{

Utilities for creating macros and working with syntax.

@defproc[(symbolic-identifier=? [stx1 syntax?] [stx2 syntax?]) boolean?]{

Compares two identifiers based on their symbolic representation.}

@defproc[(make-id [stx (U syntax? #f)] [arg (U syntax? string? symbol? number?)] ...) syntax?]{

Creates an identifier by appending @scheme[arg]@schemeidfont{s}. Equivalent to:

@schemeblock[(datum->syntax stx (string->symbol (apply string-append (map arg->string args))))]

where @scheme[arg->string] converts an argument to a string.}

@defform/subs[(begin-for-syntax/any-order definition ...)
              ([definition (define (id arg ...) expr ...)
                           (define id expr)])]{

Like @scheme[begin-for-syntax] except that definitions can refer to previous definitions in the manner of a @scheme[letrec] statement. Only definitions are allowed within the body of the form.}

} @;{end defmodule}