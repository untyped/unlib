#lang scribble/doc

@(require "base.ss")

@title[#:tag "contract"]{Contract utilities}

@(define-eval contract-eval scheme/contract (planet untyped/unlib/contract))

@defmodule[(planet untyped/unlib/contract)]{

Utilities for use with the PLT contract library.
                                                                                    
@defproc[(arity/c [arity natural?]) flat-contact?]{

Returns a flat contract that requires the input to be procedure that accepts the specified number of (non-keyword) arguments. The procedure may, in addition, accept more and/or fewer arguments and any number of keyword arguments.

@examples[
  #:eval contract-eval
  (define c (arity/c 2))
  (contract-first-order-passes? c (lambda (a b) (void)))
  (contract-first-order-passes? c (lambda (a b c) (void)))
  (contract-first-order-passes? c (lambda (a [b #f]) (void)))
  (contract-first-order-passes? c (lambda (a b #:c c) (void)))
  (contract-first-order-passes? c (lambda (a #:b b) (void)))]}

} @;{end defmodule}
