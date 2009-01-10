#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "number"]{Number utilities}

@defmodule[(planet untyped/unlib/number)]{

Useful number utilities.

@defproc[(number+false? [item any]) boolean?]{

Returns @scheme[#t] if @scheme[item] is a number or #scheme[#f].}

@defproc[(integer+false? [item any]) boolean?]{

Returns @scheme[#t] if @scheme[item] is an integer or #scheme[#f].}

@defproc[(natural? [item any]) boolean?]{

Returns @scheme[#t] if @scheme[item] is an natural number (>= 0).}

@defproc[(natural+false? [item any]) boolean?]{

Returns @scheme[#t] if @scheme[item] is an natural number (>= 0) or @scheme[#f].}

} @;{end defmodule}
