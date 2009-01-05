#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "exn"]{Exception utilities}

@(define-eval exn-eval (planet untyped/unlib/exn))

@defmodule[(planet untyped/unlib/exn)]{

Utilities for raising and handling exceptions.

@defform[(raise-exn id message arg ...)]{

Raises an exception with a default set of continuation marks. @scheme[id] is the identifier of the exception's structure type transformer binding (e.g. @scheme[exn] or @scheme[exn:fail]). @scheme[message] and @scheme[args] are passed to the exception's constructor, along with the value of @scheme[current-continuation-marks].

@examples[
  #:eval exn-eval
  (with-handlers ([exn? pretty-print])
    (raise-exn exn:fail "Oops!"))
  (with-handlers ([exn? pretty-print])
    (raise-exn exn:fail:syntax "Oops!" (list #'a #'b #'c)))]}

@defform[(reraise-exn old-exn new-exn new-message arg ...)]{

Raises @scheme[new-exn] with a message of:

@schemeblock[(string-append (exn-message old-exn) ": " new-message)]

and the same continuation marks as @scheme[old-exn]. Any additional @scheme[arg]@schemeidfont{s} are passed to the constructor of @scheme[new-exn].

@examples[
  #:eval exn-eval
  (with-handlers ([exn? pretty-print])
    (with-handlers ([exn? (lambda (e)
                            (reraise-exn e exn:fail
                              "Looks serious"))])
      (raise-exn exn "Oops!")))]}

} @;{end defmodule}
