#lang scribble/doc

@(require "base.ss")

@title[#:tag "hash"]{PLT 4x hash utilities}

@defmodule[(planet untyped/unlib/hash)]{

Useful hash utilities. These procedures are compatible with the PLT 4 procedure names for hashes.

@defproc[(make-hash/alist [data (listof pair?)]) hash?]{

Creates a mutable hash using the supplied @scheme[data]. The result has strong keys and uses @scheme[equal?] as a key comparison function.}

@defproc[(make-hasheq/alist [data (listof pair?)]) hash?]{

Creates a mutable hash using the supplied @scheme[data]. The result has strong keys and uses @scheme[eq?] as a key comparison function.}

@defproc[(make-weak-hash/alist [data (listof pair?)]) hash?]{

Creates a mutable hash using the supplied @scheme[data]. The result has weak keys and uses @scheme[equal?] as a key comparison function.}

@defproc[(make-weak-hasheq/alist [data (listof pair?)]) hash?]{

Creates a mutable hash using the supplied @scheme[data]. The result has weak keys and uses @scheme[eq?] as a key comparison function.}

@defproc[(hash-set? [hash hash?] [key any]) boolean?]{

Returns @scheme[#t] if @scheme[key] is stored in @scheme[hash], @scheme[#f] otherwise.}

@defproc[(hash-keys [hash hash?]) list?]{

Returns a (randomly ordered) list of the keys stored in @scheme[hash].}

@defproc[(hash-values [hash hash?]) list?]{

Returns a (randomly ordered) list of the values stored in @scheme[hash].}

} @;{end defmodule}