#lang scribble/doc

@(require "base.ss")

@title{@bold{Unlib:} Helpful Utilities from Untyped}

Dave Gurnell, Noel Welsh, David Brooks, and Matt Jadud

@tt{{dave, noel, djb, matt} at @link["http://www.untyped.com"]{@tt{untyped}}}

@italic{Unlib} is a collection of general programming utilities. At Untyped we mostly write web software, so expect to find useful utilities for that kind of thing.

Unlib contains more modules than are listed here. Expect anything undocumented to change or disappear in the future.

@bold{Please see the section on @secref["version-4"] for information on forthcoming backwards-incompatible changes.}

@section[#:tag "version-4"]{Changes in Unlib 4.x}

Unlib 3.x contains some legacy code that will be removed in Unlib 4.x. In most cases an upgrade path exists that will prevent future problems:

@itemize{
  @item{support for the @scheme[mzscheme] language will be dropped;}
  @item{the @filepath{enum.ss} module has been replaced with @scheme{enumeration.ss} and will be removed;}
  @item{the @filepath{hash-table.ss} module has been replaced with @filepath{hash.ss} and will be removed;}
  @item{the @filepath{cache-mzscheme.ss} module has been replaced with @filepath{cache.ss} and will be removed;}
  @item{the @filepath{string-mzscheme.ss} module has been replaced with @filepath{string.ss} and will be removed;}
  @item{the @filepath{time-mzscheme.ss} module has been replaced with @filepath{time.ss} and will be removed;}
  @item{the @filepath{enum.ss} module has been replaced with @filepath{enumeration.ss} and will be removed;}
  @item{@scheme[define-debug] has been replaced with @scheme[define/debug] and will be removed;}
  @item{@scheme[define-values-debug] has been replaced with @scheme[define-values/debug] and will be removed;}
  @item{@scheme[let-debug] has been replaced with @scheme[let/debug] and will be removed;}
  @item{@scheme[let*-debug] has been replaced with @scheme[let*/debug] and will be removed;}
  @item{@scheme[letrec-debug] has been replaced with @scheme[letrec/debug] and will be removed;}
  @item{@scheme[let-values-debug] has been replaced with @scheme[let-values/debug] and will be removed;}
  @item{@scheme[let*-values-debug] has been replaced with @scheme[let*-values/debug] and will be removed;}
  @item{@scheme[letrec-values-debug] has been replaced with @scheme[letrec-values/debug] and will be removed;}
  @item{@scheme[define-library-aliases]: the @scheme[(foo-in [a b c])] and @scheme[(foo-out [a b c])] forms bound
        by this macro have been replaced with @scheme[(foo-in a b c)] and @scheme[(foo-out a b c)]: the old forms
        will be removed.}}

@include-section{bytes.scrbl}
@include-section{cache.scrbl}
@include-section{contract.scrbl}
@include-section{crc.scrbl}
@include-section{date.scrbl}
@include-section{debug.scrbl}
@include-section{enum.scrbl}
@include-section{enumeration.scrbl}
@include-section{exn.scrbl}
@include-section{file.scrbl}
@include-section{for.scrbl}
@include-section{generator.scrbl}
@include-section{gen.scrbl}
@include-section{hash.scrbl}
@include-section{hash-table.scrbl}
@include-section{keyword.scrbl}
@include-section{log.scrbl}
@include-section{list.scrbl}
@include-section{match.scrbl}
@include-section{number.scrbl}
@include-section{parameter.scrbl}
@include-section{pipeline.scrbl}
@include-section{profile.scrbl}
@include-section{require.scrbl}
@include-section{scribble.scrbl}
@include-section{string.scrbl}
@include-section{symbol.scrbl}
@include-section{syntax.scrbl}
@include-section{time.scrbl}
@include-section{url.scrbl}
@include-section{yield.scrbl}

@section{Acknowledgements}

Many thanks to the following for their contributions: Ryan Culpepper, Eric Hanchrow, and Jay McCarthy.

