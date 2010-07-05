#lang scribble/doc

@(require "base.ss")

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

@defproc[(number+false->symbol+false [num (U number? #f)]) (U symbol? #f)]{

A version of @scheme[number->symbol] that accepts and passes through @scheme[#f].}

@defproc[(symbol+false->number+false [sym (U symbol? #f)]) (U number? #f)]{

A version of @scheme[symbol->number] that accepts and passes through @scheme[#f].}

@defproc[(number+false->string+false [num (U number? #f)]) (U string? #f)]{

A version of @scheme[number->string] that accepts and passes through @scheme[#f].}

@defproc[(string+false->number+false [str (U string? #f)]) (U number? #f)]{

A version of @scheme[string->number] that accepts and passes through @scheme[#f].}

@defproc[(round-to [num number?] [n integer? 0]) inexact?]{

Rounds @scheme[num] to @scheme[n] decimal places. @scheme[n] can be negative.}

} @;{end defmodule}
