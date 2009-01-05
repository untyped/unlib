#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "string"]{String utilities}

@(define-eval string-eval (planet untyped/unlib/string))

@defmodule[(planet untyped/unlib/string)]{

Useful string utilities. Compatible with PLT 4 languages.

@defproc[(string+false? [item any]) boolean?]{

Returns @scheme[#t] if @scheme[item] is a string and @scheme[#f] otherwise.}

@defproc[(ensure-string [item any]) any]{

Converts @scheme[bytes] arguments to @scheme[string]@schemeidfont{s}: passes all other arguments straight through.}

@defproc[(string-delimit [items (listof string?)]
                         [delimiter string?]
                         [#:prefix prefix (U string? #f) #f]
                         [#:suffix suffix (U string? #f) #f]) string?]{
                         
Similar to @scheme[string-join] from SRFI 13, except that the optional @scheme[#:prefix] and @scheme[#:suffix] arguments can be provided to add a prefix or suffix string.

@examples[
  #:eval string-eval
  (string-delimit '("1" "2" "3") ",")
  (string-delimit '("1" "2" "3") "," #:prefix "[")
  (string-delimit '("1" "2" "3") "," #:suffix "]")
  (string-delimit '("1" "2" "3") "," #:prefix "[" #:suffix "]")
  (string-delimit '("1" "2" "3") "," #:prefix #f #:suffix #f)]}

@defproc[(number+false->string+false [num (U number? #f)]) (U string? #f)]{

A version of @scheme[number->string] that accepts and passes through @scheme[#f].}

@defproc[(string+false->number+false [str (U string? #f)]) (U number? #f)]{

A version of @scheme[string->number] that accepts and passes through @scheme[#f].}

@defproc[(string+false->symbol+false [str (U string? #f)]) (U symbol? #f)]{

A version of @scheme[string->symbol] that accepts and passes through @scheme[#f].}

@defproc[(symbol+false->string+false [sym (U symbol? #f)]) (U string? #f)]{

A version of @scheme[symbol->string] that accepts and passes through @scheme[#f].}

} @;{end defmodule}
