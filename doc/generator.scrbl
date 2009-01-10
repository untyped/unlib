#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "generator"]{Generators}

@(define-eval gen-eval (file "generator.ss"))

@defmodule[(planet untyped/unlib/generator) #:use-sources ((file "generator.ss"))]{

There is no doubt that lists are useful structures for representing many kinds of data, and that @scheme[fold]@schemeidfont{s} and @scheme[map]@schemeidfont{s} are a quick, convenient way of performing arbitrary bits of list manipulation.

The main problem with the @scheme[list]/@scheme[fold]/@scheme[map] approach is the number of temporary lists generated in the process, which can take up a large amount of memory.

Generators are a half-way-house between lists and streams that aim to reduce memory overhead when large data sources are involved.

A generator is a stream-like accessor that can be repeatedly called to return new values from its source. A special "generator-end" value is returned to indicate that the source has been exhausted.

This module provides convenient ways of:

@itemize{
  @item{producing generators from lists;}
  @item{combining generators to form other generators (c.f. @scheme[fold], @scheme[map] and so on);}
  @item{accumulating results from generators (e.g. back into lists).}}

Many of the procedures defined in this module have rather unwieldy names. @filepath{gen.ss} exports versions of these procedures with shorter names: see @secref{gen} for more information.

@defthing[generator-end symbol?]{

A unique symbol that indicates a generator has finished producing values.}

@defproc[(generator-end? [item any]) boolean?]{

Predicate that recognizes @scheme[generator-end].}

@defproc[(gen-> [value-contract flat-contract?]) flat-contract?]{

Syntax that expands into a contract that is satisfied by generator procedures that produce values that satisfy @scheme[value-contract] or @scheme[generator-end?].}

@defproc[(list->generator [lis (listof a)]) (gen-> a)]{

Creates a generator that generates the values in @scheme[lis].

@examples[
  #:eval gen-eval
  (define gen
    (list->generator '(1 2)))
  (gen)
  (gen)
  (gen)]}

@defproc[(generator-map [fn (arg1 arg2 ... -> ans)]
                        [gen1 (gen-> arg1)] 
                        [gen2 (gen-> arg2)] ...) (gen-> ans)]{

The generator equivalent of @scheme[map]. Creates a generator that returns:

@schemeblock[(apply fn (list (gen1) (gen2) ...))]

The new generator ends as soon as any of @scheme[gen1 gen2 ...] end.

@examples[
  #:eval gen-eval
  (define gen
    (generator-map +
                   (list->generator '(1 2))
                   (list->generator '(2 3))))
  (gen)
  (gen)
  (gen)]}

@defproc[(generator-fold-map [fn (arg1 arg2 ... seed -> seed)]
                             [initial-seed seed]
                             [gen1 (gen-> arg1)]
                             [gen2 (gen-> arg2)]
                             ...) seed]{

A generator equivalent of @scheme[fold]. The new generator returns the values of @scheme[seed] for each application of @scheme[fn], stopping when any of the source generators stop.

@examples[
  #:eval gen-eval
  (define gen
    (generator-fold-map + 0 (list->generator '(1 2))))
  (gen)
  (gen)
  (gen)]}

@defproc[(generator-filter [pred (arg -> boolean?)] 
                           [src (gen-> arg)]) (gen-> arg)]{

Creates a generator that generates the values from @scheme[src] for which @scheme[pred] returns non-@scheme[#f].

@examples[
  #:eval gen-eval
  (define gen
    (generator-filter even? (list->generator '(1 2 3 4 5))))
  (gen)
  (gen)
  (gen)]}
  
@defproc[(generator-filter-map [fn (arg -> (U ans #f))]
                               [src (gen-> arg)]) (gen-> ans)]{

Creates a generator that generates the non-@scheme[#f] values of @scheme[(fn (src))].

@examples[
  #:eval gen-eval
  (define (test val)
    (memq val '(2 4 6)))
  (define gen
    (generator-filter-map test (list->generator '(1 2 3 4 5))))
  (gen)
  (gen)
  (gen)]}

@defproc[(generator-remove-duplicates [src (gen-> a)]
                                      [same? (a a -> boolean?) equal?])
                                      (gen-> a)]{

Creates a generator that filters out values from @scheme[src] that occur more than once in succession. The optional argument @scheme[same?] is used to test equality.

@examples[
  #:eval gen-eval
  (define gen
    (generator-remove-duplicates (list->generator '(1 2 2 3 3 3))))
  (gen)
  (gen)
  (gen)
  (gen)]}

@defproc[(generator-for-each [fn (arg1 arg2 ... -> void)]
                             [gen1 (gen-> arg1)]
                             [gen2 (gen-> arg2)]
                             ...) void?]{

Repeatedly applies @scheme[fn] for its side-effects to values form source generators @scheme[gen1 gen2 ...] until one or more sources is exhausted.

@examples[
  #:eval gen-eval
  (generator-for-each display (list->generator '(1 2 3)))]}

@defproc[(generator-fold [fn (arg1 arg2 ... seed -> seed)]
                         [initial-seed seed]
                         [gen1 (gen-> arg1)]
                         [gen2 (gen-> arg2)] ...) seed]{

Like @scheme[generator-fold-map] but only the result of the final application of @scheme[fn] is returned.

@examples[
  #:eval gen-eval
  (generator-fold + 0 (list->generator '(1 2 3)))]}

@defproc[(generator->list [src (gen-> a)]) (listof a)]{

A convenience form of generator-fold that collects the generated
values into a list (in the order generated).

@examples[
  #:eval gen-eval
  (generator->list (list->generator '(1 2 3)))]}

@defproc[(generator-project [mask (listof boolean)]
                            [src (gen-> (listof any))]
                            [same? [(any any -> boolean)] eq?])
         (gen-> (list any ... (listof (listof any))))]{

Does the equivalent of a projection (from relational algebra) on the values returned by @scheme[src]. @scheme[#t] values in @scheme[mask] correspond (by position) to "key" values in values from @scheme[src], while @scheme[#f] values correspond to "nonkey" values.

@scheme[src] is polled once and the members returned are partitioned into keys and nonkeys and stored. @scheme[src] is then polled repeatedly until it returns a list where the keys differ from those stored. At this point, the generator emits a list of the matching keys and a list of the lists of nonkeys:

@schemeblock[(list key ... (listof (list nonkey ...)))]

The optional @scheme[same?] argument is the predicate used to check key equality.

@scheme[generator-project] is useful (in conjunction with @scheme[generator-map], @scheme[map] and @scheme[match-lambda]) for iterating over sets of related results returned by database queries.

@examples[
  #:eval gen-eval
  (define src
    (list->generator (list (list "0" "0" "0" "0")
                           (list "0" "0" "0" "1")
                           123
                           (list "0" "1" "0" "0")
                           (list "0" "1" "0" "1")
                           456)))
  (define gen (generator-project (list #t #t #f #f) src equal?))
  (gen)
  (gen)
  (gen)
  (gen)
  (gen)]}

@defproc[(generator-debug [message string?] [src (gen-> a)]) (gen-> a)]{

Creates a generator that calls @scheme[debug] on each value generated.

@examples[
  #:eval gen-eval
  (define gen
    (generator-debug "message" (list->generator '(1 2))))
  (gen)
  (gen)
  (gen)]}

} @;{end defmodule}