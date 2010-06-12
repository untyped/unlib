#lang scribble/doc

@(require "base.ss")

@title[#:tag "pipeline"]{Pipelines}

@(define-eval pipeline-eval (planet untyped/unlib/pipeline))

@defmodule[(planet untyped/unlib/pipeline)]{

@italic{Pipelines} allow a programmer to wrap procedure calls in one or more pieces of useful functionality. Pipelines are lists of procedures, called @italic{stages}, each of which performs some function and calls the next stage. The last stage calls the target procedure.

An example of the usefulness of this concept is request processing in a web application. The application may consist of a number of @italic{controller procedures}, each of which serves a different page of HTML. Many of these procedures will have one or more bits of functionality in common:

@itemize{
  @item{setting up cookies;}
  @item{identifying the user's browser;}
  @item{checking the user's security privileges;}
  @item{and so on...}}

Note that, while many of these functions will be common across many controllers, there will be occasions where one controller will need to do things differently from the others.

The tasks above can be implemented as stages in a request processing pipeline. A standard pipeline can be offered site-wide, and controllers can choose to customise it where appropriate by adding, removing or changing stages.

@examples[
  #:eval pipeline-eval
  (define (stage1 continue arg1 arg2 arg3)
    (printf "Entering ... ")
    (continue arg1 arg2 arg3))
  (define (stage2 continue arg1 arg2 arg3)
    (printf "continuing ... ")
    (continue arg1 arg2 arg3))
  (define (target arg1 arg2 arg3)
    (printf "done.~n")
    (list arg1 arg2 arg3))
  (call-with-pipeline (list stage1 stage2) target 1 2 3)]

Stages take the same arguments as the target procedure, plus a "continuation procedure" that is called to continue the pipeline. The arguments passed to the continuation procedure are passed on to the next stage, and so on to the target.

Any stage can abort the pipeline simply by failing to call @schemeidfont{continue}. Stages can also set up parameters, install exception handlers, change the arguments to subsequent stages and so on.

@defproc[(call-with-pipeline [pipeline (listof procedure?)] [target procedure?] [arg any] ...) any]{

Calls @scheme[target] via the stages @scheme[pipeline]. The result returned is either the return value of @scheme[target] or that of the last stage invoked.}

} @;{end defmodule}