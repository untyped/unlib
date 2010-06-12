#lang scribble/doc

@(require "base.ss")

@title[#:tag "profile"]{Profiling tools}

@(define-eval profile-eval srfi/1 (planet untyped/unlib/profile))

@defmodule[(planet untyped/unlib/profile)]{

Simple profiling tools.

@defstruct[timer ([name symbol?])]{

Structure used to record a running total time. Only one timer can be running at a time per thread. The accumulated time associated with a timer can be retrieved using @scheme[timer-value] and printed by printing the timer.}

@defproc[(profile [timer timer?] [fn procedure?] [arg any] ...) any]{

Applies @scheme[fn] to @scheme[args] and returns the result. Measures the time taken to apply @scheme[fn] and adds it to the running total in @scheme[timer]. Timers are only updated when control passes into or out of a @scheme[profile] form.}

@examples[
  #:eval profile-eval
  (define t1 (make-timer 't1))
  (profile t1 foldl + 0 (iota 1000000))
  t1]

@defform[(with-timer timer expr ...)]{

Syntactic shorthand for:

@schemeblock[
  (profile timer (lambda () expr ...))]}

@defproc[(timer-value [timer timer?]) number?]{

Returns the current value of @scheme[timer]. Timers are only updated when control passes into or out of a @scheme[profile] form.}

@defproc[(timer-reset! [timer timer?]) void?]{

Resets @scheme[timer] to zero.}

@defproc[(current-timer) timer?]{

Returns the timer that is currently running.}

@defproc[(all-timers) (listof timer?)]{

Returns a list of all timers created with @scheme[make-timer] or @scheme[define-timer], plus a single predefined timer called @scheme['top].}

@defform[(define-timer id)]{

Syntactic shorthand for:

@schemeblock[(define id (make-timer 'id))]}

} @;{end defmodule}
