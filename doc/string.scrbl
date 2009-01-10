#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "string"]{String utilities}

@(define-eval string-eval (file "string.ss"))

@defmodule[(planet untyped/unlib/string) #:use-sources ((file "string.ss"))]{

Useful string utilities. Compatible with PLT 4 languages.

@defproc[(string+false? [item any]) boolean?]{

Returns @scheme[#t] if @scheme[item] is a string or #scheme[#f].}

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

} @;{end defmodule}
