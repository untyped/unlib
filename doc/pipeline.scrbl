#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "pipeline"]{Pipelines}

@(define-eval pipeline-eval (file "pipeline.ss"))

@defmodule[(planet untyped/unlib/pipeline) #:use-sources ((file "pipeline.ss"))]{

@italic{Pipelines} allow a programmer to wrap procedure calls in one or more pieces of useful functionality. Pipelines are lists of @italic{stages}, each of which performs some function and calls the next stage. The last stage calls the target procedure.

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
  (define-stage (stage1 continue arg1 arg2 arg3)
    (printf "Entering ... ")
    (continue arg1 arg2 arg3))
  (define-stage (stage2 continue arg1 arg2 arg3)
    (printf "continuing ... ")
    (continue arg1 arg2 arg3))
  (define (target arg1 arg2 arg3)
    (printf "done.~n")
    (list arg1 arg2 arg3))
  (call-with-pipeline (list stage1 stage2) target 1 2 3)]

Stages are named so they can be uniquely referred to when manipulating pipelines in this way. This has the added advantage that single stages can be extracted and run out of context with the rest of the pipeline.

More formally, given a target procedure:

@verbatim{     target : arg ... -> ans}

a pipeline is a list of stages:

@verbatim{     pipeline : (listof stage)}

where a stage is a name and a body procedure:

@verbatim{     stage : (struct symbol ((arg ... -> ans) arg ... -> ans))}

The procedure in the stage takes the same arguments as @tt{target}, plus a "continuation procedure" that is called to continue the pipeline. The arguments passed to the continuation procedure are passed on to the next stage, and so on to the target.

Any stage can abort the pipeline simply by failing to call the continuation procedure. It is also  reasonable for stages to set up parameters, install exception handlers, change the arguments to subsequent stages and so on.

@defstruct[stage ([name symbol?] [body procedure?]) #:inspector #f]{

A pipeline. The first argument to @scheme[body] procedure is always a continuation procedure that passes control to the next stage in the pipeline.

Stages take advantage of PLT's structures/procedure masquerading functionality: they can be called directly as if they are procedures.}

@defform[(define-stage (id arg ...) expr ...)]{

Shorthand syntax for make-stage that supplies @scheme[id] as the name of the stage. The form has support for rest arguments and PLT 4 optional and keyword arguments.}

@defproc[(call-with-pipeline [pipeline (listof stage?)] [target procedure?] [arg any] ...) any]{

Calls @scheme[target] via the stages @scheme[pipeline]. The result returned is either the return value of @scheme[target] or that of the last stage invoked.}

@defproc[(find-stage [pipeline (listof stage?)] [name symbol?]) (U stage? #f)]{

Returns the appropriately @scheme[name]@schemeidfont{d} stage in @scheme[pipeline] or @scheme[#f] if the stage is not found.}

} @;{end defmodule}