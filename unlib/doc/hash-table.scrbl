#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "hash-table"]{PLT 3x hash utilities}

@defmodule[(planet untyped/unlib/hash-table) #:use-sources ((file "hash-table.ss"))]{

Useful hash utilities. These procedures are compatible with the PLT 3 / @scheme[mzscheme] language procedure names for hashes.

@defproc[(make-hash-table/pairs [data pair] ...) hash-table?]{

Creates a hash table from the supplied @scheme[data]. The result has strong keys and uses @scheme[equal?] as a key comparison function.}

@defproc[(hash-table-mapped? [hash hash?] [key any]) boolean?]{

Returns @scheme[#t] if @scheme[key] is stored in @scheme[hash], @scheme[#f] otherwise.}

@defproc[(hash-table-keys [hash hash?]) list?]{

Returns a (randomly ordered) list of the keys stored in @scheme[hash].}

@defproc[(hash-table-values [hash hash?]) list?]{

Returns a (randomly ordered) list of the values stored in @scheme[hash].}

} @;{end defmodule}