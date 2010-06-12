#lang scribble/doc

@(require "base.ss")

@title[#:tag "symbol"]{Symbol utilities}

@(define-eval symbol-eval (planet untyped/unlib/symbol))

@defmodule[(planet untyped/unlib/symbol)]{

Useful symbol utilities.

@defproc[(symbol+false? [item any]) boolean?]{

Returns @scheme[#t] if @scheme[item] is a symbol or #scheme[#f].}

@defproc[(gensym/interned [base (U symbol? string?) "g"]) symbol?]{

Like @scheme[gensym] but returns an interned symbol that can be compared with other symbols using @scheme[eq?].

@examples[
  #:eval symbol-eval
  (define sym1 (gensym))
  sym1
  (eq? sym1 (string->symbol (symbol->string sym1)))
  (define sym2 (gensym/interned))
  sym2
  (eq? sym2 (string->symbol (symbol->string sym2)))]}

@defproc[(symbol-append [sym symbol?] ...) symbol?]{

The symbol equivalent of @scheme[string-append]. Returns an interned symbol.

@examples[
  #:eval symbol-eval
  (symbol-append 'abc 'def 'ghi)
  (symbol-append 'abc)
  (symbol-append)]}

@defproc[(symbol-length [sym symbol?]) natural?]{

The symbol equivalent of @scheme[string-length].

@examples[
  #:eval symbol-eval
  (symbol-length 'AbC123)]}

@defproc[(symbol-upcase [sym symbol?]) symbol?]{

The symbol equivalent of @scheme[string-upcase]. Returns an interned symbol.

@examples[
  #:eval symbol-eval
  (symbol-upcase 'AbC123)]}

@defproc[(symbol-downcase [sym symbol?]) symbol?]{

The symbol equivalent of @scheme[string-downcase]. Returns an interned symbol.

@examples[
  #:eval symbol-eval
  (symbol-downcase 'AbC123)]}

@defproc[(number->symbol [num number?]) symbol?]{

The symbol equivalent of @scheme[number->string]. Returns an interned symbol.

@examples[
  #:eval symbol-eval
  (number->symbol 123)
  (number->symbol (/ 1 3))]}

@defproc[(symbol->number [sym symbol?]) number?]{

The symbol equivalent of @scheme[string->number]. Returns @scheme[#f] if @scheme[sym] has no numeric equivalent.

@examples[
  #:eval symbol-eval
  (symbol->number '|123|)
  (symbol->number 'abc)]}

@defproc[(number+false->symbol+false [num (U number? #f)]) (U symbol? #f)]{

A version of @scheme[number->symbol] that accepts and passes through @scheme[#f].}

@defproc[(symbol+false->number+false [sym (U symbol? #f)]) (U number? #f)]{

A version of @scheme[symbol->number] that accepts and passes through @scheme[#f].}

@defproc[(string+false->symbol+false [str (U string? #f)]) (U symbol? #f)]{

A version of @scheme[string->symbol] that accepts and passes through @scheme[#f].}

@defproc[(symbol+false->string+false [sym (U symbol? #f)]) (U string? #f)]{

A version of @scheme[symbol->string] that accepts and passes through @scheme[#f].}

} @;{end defmodule}
