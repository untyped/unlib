#lang scribble/doc

@(require "base.ss")

@title[#:tag "string"]{String utilities}

@(define-eval string-eval (planet untyped/unlib/string))

@defmodule[(planet untyped/unlib/string)]{

Useful string utilities. Compatible with PLT 4 languages.

@defproc[(string+false? [item any]) boolean?]{

Returns @scheme[#t] if @scheme[item] is a string and @scheme[#f] otherwise.}

@defproc[(ensure-string [item any]) any]{

Converts @scheme[bytes] arguments to @scheme[string]@schemeidfont{s}: passes all other arguments straight through.}

@defproc*[([(string-length/c [max natural]) flat-contract?]
           [(string-length/c [min natural] [max natural]) flat-contract?])]{
Creates a contract that recognises strings with a length within the specified inclusive bounds.}

@defproc[(string-delimit [items (listof string?)]
                         [delimiter string?]
                         [#:prefix prefix (U string? #f) #f]
                         [#:suffix suffix (U string? #f) #f]) string?]{
                         
Similar to @scheme[string-join] from SRFI 13, except that the optional @scheme[#:prefix] and @scheme[#:suffix] arguments can be provided to add a prefix or suffix string.

@examples[
  #:eval string-eval
  (string-delimit '("1" "2" "3") ",")
  (string-delimit '("1" "2" "3") "," #:prefix "[")
  (string-delimit '("1" "2" "3") "," #:suffix "]")
  (string-delimit '("1" "2" "3") "," #:prefix "[" #:suffix "]")
  (string-delimit '("1" "2" "3") "," #:prefix #f #:suffix #f)]}
                                                                      
@defproc[(string-ellipsify [str string?] [max-length natural? 20] [ellipsis string? "..."]) string?]{
Returns a shortened version of @scheme[str] that is never longer than @scheme[max-length]. If necessary, @scheme[str] is truncated and @scheme[ellipsis] is appended.

@examples[
  #:eval string-eval
  (string-ellipsify
   "The quick brown fox jumped over the lazy dog.")
  (string-ellipsify
   "The quick brown fox.")]}

@defproc[(string-sentencecase [str string?]) string?]{
Returns a copy of @scheme[str] with the first character upcased.

@examples[
  #:eval string-eval
  (string-sentencecase
   "lowercase to capitalised")
  (string-sentencecase
   "Capitalised stays capitalised")
  (string-sentencecase
   "ALLCAPS stays ALLCAPSED")]}

@defproc[(string-titlecase* [str string?]) string?]{
Like @scheme[string-titlecase], in that the first character of each word is upcased, except that any upcase characters are left upcased.

@examples[
  #:eval string-eval
         (string-titlecase*
          "lowercase to titlecase")
         (string-titlecase*
          "InterCapsed stays interCapsed")
      (string-titlecase*
       "ALLCAPS stays ALLCAPSED")]}

@defproc[(number+false->string+false [num (U number? #f)]) (U string? #f)]{

A version of @scheme[number->string] that accepts and passes through @scheme[#f].}

@defproc[(string+false->number+false [str (U string? #f)]) (U number? #f)]{

A version of @scheme[string->number] that accepts and passes through @scheme[#f].}

@defproc[(string+false->symbol+false [str (U string? #f)]) (U symbol? #f)]{

A version of @scheme[string->symbol] that accepts and passes through @scheme[#f].}

@defproc[(symbol+false->string+false [sym (U symbol? #f)]) (U string? #f)]{

A version of @scheme[symbol->string] that accepts and passes through @scheme[#f].}

@defproc[(natural->hex-string [num natural?] [#:uppercase? uppercase? boolean? #f] [#:digits digits natural? 1] [#:prefix? prefix? boolean? #f]) natural?]{

Converts a natural number to a hexadecimal string. Arguments:

@itemize{
  @item{@scheme[uppercase?] - generate uppercase strings (the default is lowercase);}
  @item{@scheme[digits] - pad the string to a certain number of hexadecimal digits (only pads the string - does not trim it);}
  @item{@scheme[prefix?] - generate strings with a @scheme["0x"] prefix (the default is unprefixed).}}}

@defproc[(hex-string->natural [str string?] [#:prefix? prefix? boolean? #f]) natural?]{

Converts a hexadecimal string like @scheme["ff"] to a natural number. Case insensitive.

Set @scheme[prefix?] to @scheme[#t] to accept strings with a @scheme["0x"] prefix.}

} @;{end defmodule}
