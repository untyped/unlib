#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "debug"]{Debugging tools}

@(define-eval debug-eval 
   scheme/pretty
   (planet untyped/unlib/debug))

@defmodule[(planet untyped/unlib/debug)]{

Utilities for printing the runtime values of variables for debugging purposes, with minimal disruption to code structure.

@defparam[debug-enabled? val boolean?]{

Boolean parameter for enabling or disabling the printing of debugging information. Defaults to @scheme[#t].}

@defparam[current-debug-printer proc (-> string? any void?)]{

Parameter controlling the formatting of printed debugging information. Value must be a procedure that takes a message and a value and returns void. The default value prints the message and a colon on one line and pretty-prints the value (slightly indented) on subsequent lines.}

@defproc[(debug [val any]) any]{

Prints @scheme[val] and returns it transparently.

@examples[
  #:eval debug-eval
  (length (debug "square"
                 (for/list ([j '(1 2 3 4 5)])
                   (for/list ([i '(1 2 3 4 5)])
                     i))))]}

@defproc[(debug* [proc procedure?] [arg any] ...) any]{

Applies @scheme[proc] to @scheme[arg]@schemeidfont{s} and prints and returns the return value transparently.

@examples[
  #:eval debug-eval
  (add1 (debug* "message" * 2 2))]}

@defform[(define/debug id expr)]{

Expands to a @scheme[define] form that prints the value of @scheme[id] as a side effect.

@examples[
  #:eval debug-eval
  (define/debug test-data
    (+ 1 2 3))]}

@defform[(define-values/debug (id ...) expr)]{

Like @scheme[define/debug] but expands to a @scheme[define-values] form.

@examples[
  #:eval debug-eval
  (define-values/debug (a b)
    (values (+ 1 2) (+ 3 4)))]}

@defform*[((let/debug ([id expr] ...) expr ...)
           (let/debug loop-id ([id expr] ...) expr ...))]{
Expands to a @scheme[let] form that prints the value of each @scheme[id] as it is assigned. In "let loop" form, values are printed at the start of each call to @scheme[loop-id].

@examples[
  #:eval debug-eval
  (let/debug ([a 1] [b 2])
    (+ a b))]}

@defform[(let*/debug ([id expr] ...) expr ...)]{
Like @scheme[let/debug] but expands to a @scheme[let*] form.}

@defform[(letrec/debug ([id expr] ...) expr ...)]{
Like @scheme[let/debug] but expands to a @scheme[letrec] form.}

@defform[(let-values/debug ([(id ...) expr] ...) expr ...)]{
Expands to a @scheme[let-values] form that prints the value of each @scheme[id] as it is assigned.

@examples[
  #:eval debug-eval
  (let-values/debug ([(a b) (values 1 2)] [(c d) (values 3 4)])
    (+ a b c d))]}

@defform[(let*-values/debug ([(id ...) expr] ...) expr ...)]{
Like @scheme[let-values/debug] but expands to a @scheme[let*-values] form.}

@defform[(letrec-values/debug ([(id ...) expr] ...) expr ...)]{
Like @scheme[letrec-values/debug] but expands to a @scheme[letrec-values] form.}

@defform[(with-pretty-indent prefix expr ...)]{

Parameterizes the @scheme[pretty-print-print-line] parameter to a procedure that acts the same as the default, except that every line is prefixed with @scheme[prefix]. @scheme[prefix] must be a string.

@examples[
  #:eval debug-eval
  (define square
    (for/list ([j '(1 2 3 4 5)])
      (for/list ([i '(1 2 3 4 5)])
        i)))
  (pretty-print square)
  (with-pretty-indent "..."
    (pretty-print square))]}
    
@defproc[(exn-context [exn exn?]) (listof symbol?)]{

Returns a printable form of the continuation marks of @scheme[exn] that can can be used with @scheme[pretty-print] to produce simple, legible debugging output.}

@defform[(debug-in string require-spec)]{

@scheme[require] form that behaves like @scheme[require-spec] but prints the imported identifiers using @scheme[debug].}

@defform[(debug-out string require-spec)]{

@scheme[provide] form that behaves like @scheme[provide-spec] but prints the exported identifiers using @scheme[debug].}

} @;{end defmodule}
