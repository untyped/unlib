#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "gen"]{Generators (short names)}

@defmodule[(planet untyped/unlib/gen)]{

This module re-provides the procedures from @filepath{generator.ss} with shorter, more convenient names. See @secref{generator} for more detailed documentation.

@defthing[g:end symbol?]{

Shorter name for @scheme[generator-end].}

@defproc[(g:end? [item any]) boolean?]{

Shorter name for @scheme[generator-end?].}

@defproc[(gen-> [value-contract flat-contract?]) flat-contract?]

@defproc[(list->generator [lis (listof a)]) (gen-> a)]

@defproc[(g:map [fn   (arg1 arg2 ... -> ans)]
                [gen1 (gen-> arg1)] 
                [gen2 (gen-> arg2)] ...) (gen-> ans)]{

Shorter name for @scheme[generator-map].}

@defproc[(g:fold-map [fn (arg1 arg2 ... seed -> seed)]
                     [initial-seed seed]
                     [gen1 (gen-> arg1)]
                     [gen2 (gen-> arg2)]
                     ...) seed]{

Shorter name for @scheme[generator-fold-map].}

@defproc[(g:filter [pred (arg -> boolean?)] 
                   [src  (gen-> arg)]) (gen-> arg)]{

Shorter name for @scheme[generator-filter].}
  
@defproc[(g:filter-map [fn  (arg -> (U ans #f))]
                       [src (gen-> arg)]) (gen-> ans)]{

Shorter name for @scheme[generator-filter-map].}

@defproc[(g:remove-duplicates [src (gen-> a)]
                              [same? (a a -> boolean?) equal?])
                              (gen-> a)]{
Shorter name for @scheme[generator-remove-duplicates].}

@defproc[(g:for-each [fn (arg1 arg2 ... -> void)]
                     [gen1 (gen-> arg1)]
                     [gen2 (gen-> arg2)]
                     ...) void?]{

Shorter name for @scheme[generator-for-each]}

@defproc[(g:fold [fn (arg1 arg2 ... seed -> seed)]
                 [initial-seed seed]
                 [gen1 (gen-> arg1)]
                 [gen2 (gen-> arg2)] ...) seed]{

Shorter name for @scheme[generator-fold]}

@defproc[(g:collect [src (gen-> a)]) (listof a)]{

Shorter name for @scheme[generator-fold].}

@defproc[(g:project [mask  (listof boolean)]
                    [src   (gen-> (listof any))]
                    [same? [(any any -> boolean)] eq?])
         (gen-> (list any ... (listof (listof any))))]{

Shorter name for @scheme[generator-project]}

@defproc[(g:debug [message string?] [src (gen-> a)]) (gen-> a)]{

Shorter name for @scheme[generator-debug].}

} @;{end defmodule}