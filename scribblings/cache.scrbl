
#lang scribble/doc

@(require "base.ss")

@title[#:tag "cache"]{Write-through cache}

@defmodule[(planet untyped/unlib/cache)]{

A @italic{write-through cache} is essentially a hash table that calls user supplied functions to load and store data from a @italic{back-end store} (a database, file, network resource or other). This is useful for, for example, caching database values so that writes are always sent to the database (so it is always up-to-date) but data is only loaded as needed.

The cache associates a fixed life-span to all stored data, so data in the cache will eventually be removed.  This ensures data is the cache does not get too old, and additionally that memory consumption is constrained.

@defproc[(make-cache [load-proc  (-> key value)]
                     [store-proc (-> key value void?)]
                     [#:expire expire-proc (-> cache? key value void?) void]
                     [#:lifetime lifetime natural? 3600]) cache?]{

Creates a write-through cache using the given @scheme[load-proc] and @scheme[store-proc] and @scheme[equal?] has a key comparison function.

@scheme[load-proc] is called by @scheme[cache-ref] when there is no cached value for a given key. It is passed the key and must return the corresponding value from the back-end store. @scheme[cache-ref] updates the cache with the loaded value: subsequent requests for the same key will not trigger @scheme[load-proc] until the cached value expires.

@scheme[store-proc] is called by @scheme[cache-set!]. It is passed a key and value and must update the back-end store. @scheme[cache-set!] updates the cache automatically.

The optional @scheme[expire-proc] is called immediately after a value is removed from the cache (either manually or through natural expiry). The function is passed a reference to the cache, the key and the expiring value. The default function does nothing but user-supplied functions can, for example, add the data back to the cache or re-query the back-end store.

The optional @scheme[lifetime] specifies the number of seconds that each item is stored in the cache.

Items are expired in two ways:

@itemize{
  @item{a thread makes sweeps of the entire cache once every @scheme[(/ lifetime 2)] seconds;}
  @item{@scheme[cache-ref] checks if a requested key has expired since the most recent sweep.}}}

@defproc[(make-cacheeq [load-proc  (-> key value)]
                       [store-proc (-> key value void?)]
                       [#:expire expire-proc (-> cache? key value void?) void]
                       [#:lifetime lifetime natural? 3600]) cache?]{

Like @scheme[make-cache] but uses @scheme[eq?] as the key comparison function.}

@defproc[(cache? [item any]) boolean?]{

Predicate for identifying caches. Returns @scheme[#t] for caches created with @scheme[make-cache] and @scheme[make-cacheeq].}

@defproc[(cache-eq? [cache cache?]) boolean?]{

Returns @scheme[#t] if @scheme[cache] uses @scheme[eq?] as its key comparison function (i.e. it was created with @scheme[make-cacheeq] rather than @scheme[make-cache]).}

@defproc[(cache-ref [cache cache?] [key key]) value]{

Retrieves a value from @scheme[cache]. If the cache contains a value for @scheme[key], the value is returned immediately. If the cache does not contain a value for @scheme[key] (or the value has recently expired; see @scheme[make-cache] for more information), its @scheme[load-proc] is called to retrieve the value from its back-end store.}

@defproc[(cache-set! [cache cache?] [key key] [value value]) void?]{

Stores the supplied @scheme[key]/@scheme[value] pair in @scheme[cache] and calls its @scheme[store-proc] to update its back-end store.}

@defproc[(cache-clear! [cache cache?] [key key]) void?]{

Removes all values from @scheme[cache], calling its @scheme[expire-proc] for each value.}

} @;{end defmodule}