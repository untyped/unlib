#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "crc"]{CRC checksums}

@(define-eval crc-eval (planet untyped/unlib/crc))

@defmodule[(planet untyped/unlib/crc)]{

@defproc[(crc32 [data bytes?]) natural?]{

Returns the IEEE 32-bit checksum of @scheme[data].

@examples[
  #:eval crc-eval
  (crc32 #"Hello world!")
  (crc32 #"Hello world.")]}

} @;{end defmodule}