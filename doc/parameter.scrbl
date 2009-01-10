#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "parameter"]{Parameter utilities}

@(define-eval param-eval (file "number.ss") (file "parameter.ss"))

@defmodule[(planet untyped/unlib/parameter) #:use-sources ((file "parameter.ss"))]{

Convenience forms for working with parameters.

@defproc[(make-guard [pred (any -> boolean?)] [type-message string?]) (any -> any)]{

Creates a procedure that may be used as a parameter's @italic{guard procedure}. The guard only allows values for which @scheme[pred] returns @scheme[#t]. If an invalid value is supplied, the guard raises @scheme[exn:fail:contract] with an error message based on the supplied @scheme[type-message].

@scheme[make-guard] has been superseded by the @scheme[parameter/c] contract in PLT 4.

@examples[
  #:eval param-eval
  (define param 
    (make-parameter #f (make-guard integer+false? "(U integer #f)")))
  (param 1)
  (param #f)
  (param #t)]}

@defform[(define-parameter id initial-value guard-proc with-form-id)]{

Convenience form that expands into two definitions:

@itemize{
  @item{@scheme[id] is bound to a parameter with the supplied @scheme[initial-value] and @scheme[guard-proc];}
  @item{@scheme[with-form-id] is bound to a syntax of the form:
  
    @schemeblock[(with-form-id expr
                    body ...)]
                    
    that expands to:
    
    @schemeblock[(parameterize ([id expr]) body ...)]}}

@examples[
  #:eval param-eval
  (define-parameter foo 
    #f 
    (make-guard integer+false? "(U integer #f)")
    with-foo)
  (with-foo 10 (foo))
  (with-foo "bar" 10)]}

} @;{end defmodule}