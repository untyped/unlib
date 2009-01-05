#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "debug"]{Debugging tools}

@(define-eval debug-eval srfi/1/list (file "debug.ss"))

@defmodule[(planet untyped/unlib/debug) #:use-sources ((file "debug.ss"))]{

Utilities for printing the runtime values of variables for debugging purposes, with minimal disruption to code structure.

@defparam[debug-enabled? val boolean?]{

Boolean parameter for enabling or disabling the printing of debugging information. Defaults to @scheme[#t].}

@defparam[current-debug-printer proc (-> string? any void?)]{

Parameter controlling the formatting of printed debugging information. Value must be a procedure that takes a message and a value and returns void. The default value prints the message and a colon on one line and pretty-prints the value (slightly indented) on subsequent lines.}

@defproc[(debug [val any]) any]{

Prints @scheme[val] and returns it transparently.

@examples[
  #:eval debug-eval
  (length (debug "message" (make-list 5 (iota 10))))]}

@defproc[(debug* [proc procedure?] [arg any] ...) any]{

Applies @scheme[proc] to @scheme[arg]@schemeidfont{s} and prints and returns the return value transparently.

@examples[
  #:eval debug-eval
  (debug* "message" * 2 2)]}

@defform[(define-debug id expr)]{

Expands to a @scheme[define] form that prints the value of @scheme[id] as a side effect.

@examples[
  #:eval debug-eval
  (define-debug test-data
    (+ 1 2 3))]}


@defform[(let-debug ([id expr] ...) expr ...)]{

Expands to a @scheme[let] form that prints the value of each @scheme[id] as it is assigned.

@examples[
  #:eval debug-eval
  (let-debug ([a (+ 1 2)]
              [b (+ 3 4)])
     b)]}

@defform[(let*-debug ([id expr] ...) expr ...)]{

Expands to a @scheme[let*] form that prints the value of each @scheme[id] as it is assigned.

@examples[
  #:eval debug-eval
  (let*-debug ([a (+ 1 2)]
               [b (* b b)])
     b)]}

@defform[(letrec-debug ([id expr] ...) expr ...)]{

Expands to a @scheme[letrec] form that prints the value of each @scheme[id] as it is assigned.

@examples[
  #:eval debug-eval
  (let-debug ([a (+ 1 2)]
              [b (* b b)])
     (append a b))]}

} @;{end defmodule}
