#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "file"]{File and path utilities}

@defmodule[(planet untyped/unlib/file)]{

Utilities for manipulating files and paths.

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

} @;{end defmodule}
