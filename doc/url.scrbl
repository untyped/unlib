#lang scribble/doc

@(require (file "base.ss"))

@(define-eval url-eval net/url (planet untyped/unlib/url))

@title[#:tag "url"]{URL utilities}

@defmodule[(planet untyped/unlib/url)]{

Useful URL utilities.

@defproc[(url-remove-params [url url?]) url?]{

Returns a copy of @scheme[url] with the @italic{param} parts of the path removed.

@examples[
  #:eval url-eval
  (url->string (remove-url-params (string->url "http://www.example.com/abc/def/ghi")))
  (url->string (remove-url-params (string->url "/a;1/b;2/c;3")))
  (url->string (remove-url-params (string->url "/;1*2*345678")))]}

@defproc[(url-local [url url?]) url?]{

Returns a copy of @scheme[url] with the scheme, user and hostname and port removed.

@examples[
  #:eval url-eval
  (url->string (url-local (string->url "http://www.example.com/abc/def/ghi")))
  (url->string (url-local (string->url "/a;1/b;2/c;3")))
  (url->string (url-local (string->url "/;1*2*345678")))]}

@defproc[(url-path-only [url url?]) url?]{

Like @scheme[url-local] but removes the query and anchor strings as well.

@examples[
  #:eval url-eval
  (url->string (url-path-only (string->url "http://www.example.com/abc/def/ghi")))
  (url->string (url-path-only (string->url "/a;1/b;2/c;3")))
  (url->string (url-path-only (string->url "/;1*2*345678")))]}

} @;{end defmodule}
