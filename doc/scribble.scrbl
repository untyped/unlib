#lang scribble/doc

@(require (file "base.ss")
          (for-label scribble/eval))

@title[#:tag "scribble"]{Scribble utilities}

@defmodule[(planet untyped/unlib/scribble)]{

Useful Scribble utilities.

@defform[(define-eval id require-spec ...)]{

Defines a sandboxed eval environment containing bindings from @scheme[scheme/base], @scheme[scheme/pretty] and whatever @scheme[require-spec]@schemeidfont{s} are listed in the form. Display width is limited to 40 columns, which seems to be about right for the default Scribble documentation fonts.}

} @;{end defmodule}
