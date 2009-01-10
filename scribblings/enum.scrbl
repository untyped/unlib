#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "enum"]{Enumerations}

@(define-eval enum-eval (planet untyped/unlib/enum))

@defmodule[(planet untyped/unlib/enum)]{

Utilities for defining simple enumerations of booleans, symbols and integers. These are useful wherever you would normally use a small collection of literals to represent possible values of a variable, and test for value equality with @scheme[eq?]. The @scheme[define-enum] form binds the literals to Scheme identifiers so the compiler catches typos that might otherwise take a long time to debug.

@defstruct[enum ([name symbol?]
                 [values        (listof (U boolean? symbol? integer?))]
                 [pretty-values (listof string?)])]{
An enumeration. For each symbol in @scheme[values] there is a human-readable string equivalent in @scheme[pretty-values].}
                                                   
@defproc[(enum->string [enum enum?]) string?]{
Returns a string representation of @scheme[(enum-values enum)], useful for including in debugging output.
        
@examples[
  #:eval enum-eval
  (define-enum vehicle (car boat plane))
  (enum->string vehicle)]}

@defproc[(enum->pretty-string [enum enum?]) string?]{
Returns a string representation of @scheme[(enum-pretty-values enum)], useful for describing the possible values to a user.
        
@examples[
  #:eval enum-eval
  (define-enum vehicle (car boat plane))
  (enum->pretty-string vehicle)]}

@defproc[(enum-value? [enum enum?] [value any]) boolean?]{
Returns @scheme[#t] if @scheme[value] is a member of @scheme[(enum-values enum)].

@examples[
  #:eval enum-eval
  (define-enum vehicle (car boat plane))
  (enum-value? vehicle 'car)
  (enum-value? vehicle 'apple)]}

@defproc[(enum-prettify [enum    enum?]
                        [value   symbol?]
                        [default (U string? (-> string?)) (cut raise-exn exn:fail:contract ...)])
         string?]{
Returns the pretty equivalent of @scheme[value]. If @scheme[value] is not found in @scheme[enum], @scheme[default] is used instead:

@itemize{
  @item{if @scheme[default] is a procedure, it is called to determine the return value;}
  @item{if @scheme[default] is not a procedure, it is returned.}}}
       
@defform/subs[(define-enum enum-id (value-clause ...) keyword-arg ...)
              ([value-clause value-id
                             [value-id pretty-expr]
                             [value-id value-expr pretty-expr]]
               [value-expr   (U boolean? symbol? integer?)]
               [pretty-expr  string?]
               [keyword-arg (code:line #:plural id)])]{
Binds the following identifiers:

@itemize{
  @item{@scheme[enum-id]: an enumeration struct;}  
  @item{@scheme[value-id] (one binding per value): the values of the enumeration, each a symbol;}
  @item{@scheme[enum-id]@schemeidfont{?}: a predicate that recognises the values;}
  @item{@scheme[enum-id]@schemeidfont{-out}: a provide form that provides all of the above.}}

If @scheme[value-expr] and @scheme[pretty-expr] are unspecified for a value, they are created from @scheme[value-id].
 
@examples[
  #:eval enum-eval
  (define-enum vehicle (car boat plane))
  car
  boat
  plane
  (vehicle? car)
  (vehicle? 'apple)]

The optional @scheme[#:prefix] argument affects the names of the value identifiers (@scheme[car], @scheme[boat] and so on) bound by the macro:

@examples[
  #:eval enum-eval
  (define-enum vehicle (car boat plane) #:prefix vehicle-)
  vehicle-car
  vehicle-boat
  vehicle-plane]}

} @;{end defmodule}