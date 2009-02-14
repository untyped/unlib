#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "require"]{Require shortcuts}

@defmodule[(planet untyped/unlib/require)]{

Shortcuts for use in @scheme[require] statements.

@defform[(directory-in path)]{
Expands to @scheme[(combine-in (file "foo.ss") ...)] for all Scheme source files (@filepath{.ss} and @filepath{.scm} extensions) in @scheme[path]. @scheme[path] must be a string literal.

Be aware that this form is sensitive to the value of @scheme[current-directory] and may not be useful in all cases. Future improvements will bind the form to the directory containing the current module.}

@defform[(define-file-require-syntax id directory)]{
Binds @scheme[id] to a procedure that generates require statements for modules in a local @scheme[directory]. @scheme[directory] must be a string literal. Modules must have a @filepath{.ss} extension. For example:

@schemeblock[
  (define-file-require-syntax foo-in "foo")
  
  (code:comment "Require foo/bar.ss:")
  (require (foo-in bar))
  
  (code:comment "Require foo/bar/baz.ss:")
  (require (foo-in bar/baz))]}

@defform[(define-planet-require-syntax id shorthand-package-spec)]{
Binds @scheme[id] to a procedure that generates require statements for modules in the PLaneT package with the specified @scheme[shorthand-package-spec]. For example:

@schemeblock[
  (define-planet-require-syntax foo-in untyped/foo:1:0)
  
  (code:comment "Require untyped/foo:1:0/bar.ss:")
  (require (foo-in bar))
  
  (code:comment "Require untyped/foo:1:0/bar/baz.ss:")
  (require (foo-in bar/baz))]}

} @;{end defmodule}
