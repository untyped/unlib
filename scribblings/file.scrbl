#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "file"]{File and path utilities}

@(define-eval file-eval (planet untyped/unlib/file))

@defmodule[(planet untyped/unlib/file)]{

Utilities for manipulating files and paths.

@defproc[(path-contains? [path1 path?] [path2 path?]) boolean?]{

Determines whether @scheme[path2] is a subpath of @scheme[path1].

@examples[
  #:eval file-eval
  (path-contains? (build-path "/a/b/c")
                  (build-path "/a/b/c/d"))
  (path-contains? (build-path "/a/b/c/d")
                  (build-path "/a/b/c"))
  (path-contains? (build-path "/a/b/c")
                  (build-path "/a/b/c/d/../../d"))
  (path-contains? (build-path "/a/b/c")
                  (build-path "/a/b/c/d/../../c/d"))]}

@defproc[(make-directory-tree [tree folders-spec]) void?]{

Creates a directory tree in the current directory that matches @scheme[tree], which is a tree of strings of type @scheme[folders-spec]:

@verbatim{
  folders-spec ::= (listof folder-spec)
  folder-spec  ::= string folders-spec}

@scheme[tree-spec] For example, the code:

@schemeblock[(make-directory-tree '("a" ("b" "c" ("d"))))]

would create the directory tree:

@verbatim{
  /a
  /a/b
  /a/c
  /a/c/d}

Any existing directories in the tree are left intact.}

@defproc[(make-non-conflicting-filename [path (U path? string?)] [filename string?]) string?]{

Returns a filename that is guaranteed to not conflict with the names of any files in @scheme[path]. For example:

@schemeblock[(make-non-conflicting-filename (string->path "mydir") "myfile.txt")]

would return:

@itemize{
  @item{@scheme{myfile.txt} if @scheme{myfile.txt} doesn't exist in @scheme{mydir};}
  @item{@scheme{myfile1.txt} if @scheme{myfile.txt} does exist in @scheme{mydir};}
  @item{@scheme{myfile2.txt} if @scheme{myfile.txt} and @scheme{myfile1.txt} both exist in @scheme{mydir};}
  @item{and so on...}}}

@defproc[(make-non-conflicting-path [path (U path? string?)] [filename string?]) path?]{

Like @scheme[make-non-conflicting-filename] but returns:

@schemeblock[(build-path path (make-non-conflicting-filename path filename))]}

@defproc[(read-file->string [file (U path? string?)]) string?]{

Reads the contents of @scheme[file] into a string. See the @tt{port.plt} collection on PLaneT for more advanced functions along these lines.}

@defproc[(concatenate-files [dest    (U path? string?)] 
                            [sources (listof (U path? string?))]) void?]{
                            
Concatenates (appends) the contents of @scheme[sources] and writes the result to @scheme[dest].}

@defproc[(directory-tree [path (U path? string?)]
                         [#:order order (U 'pre 'post) 'pre]
                         [#:filter filter (path? -> boolean?) (lambda (path) #t)]
                         [#:follow-links? follow-links? boolean? #t]) (listof path?)]{
Returns a list of absolute paths of all matching files/directories/links in @scheme[path] or subdirectories thereof.

The @scheme[order] argument specifies whether the directory tree should be traversed in pre- or post-order; the @scheme[filter] argument specifies a predicate which returned results much match; the @scheme[follow-links?] argument specifies whether links to directories should be treated as directories or atomic files.}

@defproc[(in-directory [path (U path? string?)]
                       [#:order order (U 'pre 'post) 'pre]
                       [#:filter filter (path? -> boolean?) (lambda (path) #t)]
                       [#:follow-links? follow-links? boolean? #t]) sequence?]{
A wrapper for @scheme[directory-tree] that returns a sequence that is compatible with @scheme[for] and its equivalents.}

@defproc[(file-pretty-size [file (U path? string?)]) string?]{
Returns the size of @scheme[file], formatted in a humane way. See @scheme[perttify-file-size] for examples.}

@defproc[(prettify-file-size [size natural?]) string?]{
Like @scheme[file-pretty-size] but takes the @scheme[size] of the file as an integer.

@examples[
  #:eval file-eval
  (prettify-file-size 500)
  (prettify-file-size (* 500 1024))
  (prettify-file-size (* 1000 1024))
  (prettify-file-size (* 100 1024 1024 1024))]}

} @;{end defmodule}
