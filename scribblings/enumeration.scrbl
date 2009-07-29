#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "enumeration"]{Enumerations (revised)}

@(define-eval enum-eval (planet untyped/unlib/enumeration))

@defmodule[(planet untyped/unlib/enumeration)]{

Utilities for defining simple enumerations of booleans, symbols and integers. These are useful wherever you would normally use a small collection of literals to represent possible values of a variable, and test for value equality with @scheme[eq?]. The @scheme[define-enum] form binds the literals to Scheme identifiers so the compiler catches typos that might otherwise take a long time to debug.

@defstruct[enum ([name symbol?]
                 [values        (listof (U boolean? symbol? integer?))]
                 [pretty-values (listof string?)])]{
An enumeration. For each symbol in @scheme[values] there is a human-readable string equivalent in @scheme[pretty-values].}
                                                   
@defform/subs[#:literals (_) (define-enum enum-id (value-clause ...) keyword-arg ...)
              ([value-clause value-id
                             [value-id pretty-expr]
                             [value-id value-expr pretty-expr]]
               [value-expr   any
                             _]
               [pretty-expr  string?]
               [keyword-arg (code:line #:plural id)])]{
Binds @scheme[enum-id] to an @italic{enumeration macro} that can be used:

@itemize{
  @item{in argument position to refer to an @scheme[enum] struct;}
  @item{in procedure call position to retrieve a value from the enumeration.}}

@examples[
  #:eval enum-eval
  (define-enum options
    ([a 'option1 "first option"]
     [b "second option"]
     c))
  (options a)
  (options b)
  (options c)
  (map (lambda (val)
         (enum-prettify options val))
       (list (options a) (options b) (options c)))]

The identifier @scheme[_] can be used as a @scheme[value-expr] as a shorthand for @scheme['value-id]. This is useful when writing value clauses where the @scheme[value-id] and the @scheme[pretty-expr] are the most important parts.

@examples[
  #:eval enum-eval
  (define-enum options
    ([a _ "first option"]
     [b _ "second option"]
     [c _ "third option"]))
  (options a)
  (options b)
  (options c)
  (map (lambda (val)
         (enum-prettify options val))
       (list (options a) (options b) (options c)))]}
  
@defproc[(enum->string [enum enum?] [separator string? ", "]) string?]{
Returns a string representation of @scheme[(enum-values enum)], useful for including in debugging output. @scheme[separator] is used to separate the enum values in the return value.
        
@examples[
  #:eval enum-eval
  (define-enum vehicle (car boat plane))
  (enum->string vehicle)]}

@defproc[(enum->pretty-string [enum enum?] [separator string? ", "]) string?]{
Returns a string representation of @scheme[(enum-pretty-values enum)], useful for describing the possible values to a user. @scheme[separator] is used to separate the enum values in the return value.

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

@defform[(enum-list enum value ...)]{
Expands to a @scheme[list] of @scheme[value]@schemeidfont{s} from @scheme[enum].

@examples[
  #:eval enum-eval
  (define-enum vehicles (car boat plane))
  (enum-list vehicles car boat)]}

@defform/subs[#:literals (else) (enum-case enum value clause ...)
              ([clause [(value ...) expr ...]
                       [else expr ...]])]{
Like @scheme[case] but each @scheme[value] must be a value from @scheme[enum]. If an @scheme[else] expression is not provided, the @scheme[value]@schemeidfont{s} must cover the complete enumeration.

@examples[
  #:eval enum-eval
  (define-enum vehicles (car boat plane))
  (define (flies? vehicle)
    (enum-case vehicles vehicle
      [(plane) #t]
      [(car boat) #f]))
  (flies? 'car)
  (flies? 'plane)]}

} @;{end defmodule}