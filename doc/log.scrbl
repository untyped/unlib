#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "log"]{Logging tools}

@(define-eval log-eval (planet untyped/unlib/log))

@defmodule[(planet untyped/unlib/log)]{

Utilities for simple logging of messages in an application-specific format.

@defstruct[log-stream ([name symbol?])]{

A structure representing a particular stream of log messages.}

@defthing[message-log log-stream?]{

Default log stream for normal messages.}

@defthing[warning-log log-stream?]{

Default log stream for warning messages.}

@defthing[error-log log-stream?]{

Default log stream for error messages.}

@defparam[current-log-preamble preamble-ref (-> list?)]{

Parameter for controlling the standard preamble printed with each log message. @scheme[preamble-ref] is thunk that  returns a list of values to be included at the begining of each log message.

@examples[
  #:eval log-eval
  (log-message "string" 123 'symbol)
  (parameterize ([current-log-preamble
                  (lambda () (list (current-inexact-milliseconds)))])
    (log-message "string" 123 'symbol)
    (log-message "string" 123 'symbol))]}

@defform[(with-log-preamble thunk expr ...)]{

Syntactic shorthand for:

@schemeblock[
  (parameterize ([current-log-preamble thunk])
    expr ...)]}

@defparam[current-log-port port+thunk (U output-port? (-> output-port?))]{

Parameter that controls the output-port to which to print log messages. @scheme[port+thunk] is either an output port or a thunk that returns an output port (useful, for example, for tying @scheme[current-log-port] to @scheme[current-output-port] or @scheme[current-error-port]).}

@defform[(with-log-port port+thunk expr ...)]{

Syntactic shorthand for:

@schemeblock[
  (parameterize ([current-log-port port+thunk])
    expr ...)]}

@defproc[(log-generic [log log-stream?] [args list]) time?]{

Prints a message to @scheme[log] containing the stream name, the @scheme[current-log-preamble] and any @scheme[args], and returns the current time as a SRFI 19 time structure.

@examples[
  #:eval log-eval
  (log-generic message-log (list "string" 123 'symbol))
  (log-generic warning-log (list "string" 123 'symbol))
  (log-generic error-log (list "string" 123 'symbol))]}

@defproc[(log-message [arg any] ...) time?]{

Shorthand for @scheme[(log-generic message-log (list any ...))].}

@defproc[(log-warning [arg any] ...) time?]{

Shorthand for @scheme[(log-generic warning-log (list any ...))].}

@defproc[(log-error [arg any] ...) time?]{

Shorthand for @scheme[(log-generic error-log (list any ...))].}

} @;{end defmodule}