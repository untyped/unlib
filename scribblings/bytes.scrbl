#lang scribble/doc

@(require "base.ss")

@title[#:tag "bytes"]{Bytes utilities}

@defmodule[(planet untyped/unlib/bytes)]{

Useful bytes utilities.

@defproc[(bytes+false? [item any]) boolean?]{

Returns @scheme[#t] if @scheme[item] is a bytes or @scheme[#f], and @scheme[#f] otherwise.}

@defproc[(ensure-bytes [item any]) any]{

Converts @scheme[string] arguments to @scheme[bytes]: passes all other arguments straight through.}

} @;{end defmodule}
