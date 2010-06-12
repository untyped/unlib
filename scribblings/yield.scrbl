#lang scribble/doc

@(require "base.ss")

@title[#:tag "yield"]{Yieldable procedures}

@(define-eval yield-eval (planet untyped/unlib/yield))

@defmodule[(planet untyped/unlib/yield)]{

Implements the "yield" operator of Ruby / Python using continuations. The "yield" command pauses the execution of a procedure and returns a result. Execution continues from the same point in the next invocation of the procedure (rather from the beginning of the procedure as usual).

Supports procedures with multiple arguments and return types.

@examples[
  #:eval yield-eval
  (define calc
    (make-yieldable
     (lambda (yield)
       (lambda (a b)
         (define-values (c d)
           (yield a b))
         (values (* c 2) (* d 2))))))
  (calc 1 2)
  (calc 1 2)]

@defproc[(make-yieldable [yield->proc (yield-proc -> target-proc)]) target-proc]{

Creates a target procedure that can use @scheme[yield-proc] to suspend operation. @scheme[yield-proc] and @scheme[target-proc] have symmetric contracts:

@verbatim{
    yield-proc : a b c -> d e
    target-proc : d e -> a b c}}

@defform[(yieldable yield-id expr ...)]{

A syntactic form of @scheme[make-yieldable] that avoids writing so many @scheme[lambda]@schemeidfont{s}.

@examples[
  #:eval yield-eval
  (define calc
    (yieldable yield
      (lambda (a b)
        (define-values (c d)
          (yield a b))
        (values (* c 2) (* d 2)))))
  (calc 1 2)
  (calc 1 2)]}

} @;{end defmodule}