#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "require"]{Require utilities}

@defmodule[(planet untyped/unlib/require)]{

Utilities for use with @scheme[require] statements.

@defform[(directory-in path)]{
Expands to @scheme[(combine-in (file "foo.ss") ...)] for all Scheme source files (@filepath{.ss} and @filepath{.scm} extensions) in @scheme[path]. @scheme[path] must be a string literal.

@italic{Known issues:} This form is sensitive to the value of @scheme[current-directory] and may not be useful in all cases. Future improvements will force @scheme[path] to be relative to the directory containing the current module.}

@defform*/subs[#:literals (file planet string id)
               ((define-library-aliases id source kw ...)
                (define-library-aliases (in-id out-id) source kw ...))
               ([source      (file dir-spec)
                             (planet planet-spec)]
                [dir-spec    string]
                [planet-spec id]
                [kw          #:provide])]{
Defines @scheme[require] and @scheme[provide] shortcuts for a code library. Similar in function to Ryan Culpepper's @link{http://planet.plt-scheme.org/display.ss?package=require.plt&owner=ryanc}{Require.plt}.

The two-identifier form binds @scheme[in-id] and @scheme[out-id] to require- and provide-transformers that require and provide modules from the specified library. The single-identifier form expands to the two-identifier form by appending @schemeidfont{-in} and @schemeidfont{-out} to @scheme[id]. If the @scheme[#:provide] keyword is specified, @scheme[provide] statements are automatically injected for @scheme[in-id] and @scheme[out-id].

@scheme[dir-spec] must be a string literal, which is expanded to a path using:

@schemeblock[
  (path->complete-path (expand-user-path (build-path dir-spec)))]

This means platform-specific shorthands such as @scheme{~} are valid in directory names. @scheme[planet-spec] must be a shorthand PLaneT package name. Module filenames must end with @filepath{.ss}.

Examples:

@schemeblock[
  (code:comment "Define (and provide) a-in and a-out:")
  (define-library-aliases a (file "foo") #:provide)
  
  (require (a-in)       (code:comment "require a/main.ss")
           (a-in [b c]) (code:comment "require a/b.ss and a/c.ss")
           (a-in d/e))  (code:comment "require a/d/e.ss")
  
  (code:comment "Define (but do not provide) x-in and x-out:")
  (define-library-aliases x (planet untyped/bar:1:2))
  
  (require (x-in a))  (code:comment "require untyped/bar:1:2/a.ss")
  (provide (x-out a)) (code:comment "provide everything from untyped/bar:1:2/a.ss")]}

} @;{end defmodule}
