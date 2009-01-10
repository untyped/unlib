#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "list"]{List utilities}

@(define-eval list-eval (file "list.ss"))

@defmodule[(planet untyped/unlib/list) #:use-sources ((file "list.ss"))]{

Utilities for lists.} @;{end defmodule}

@section{Regular lists}

@defproc[(list-swap [lis list?] [index1 natural?] [index2 natural?]) list?]{

Returns a copy of @scheme[lis] with the values at @scheme[index1] and @scheme[index2] swapped over.

@examples[
  #:eval list-eval
  (list-swap '(1 2 3 4 5) 1 3)
  (list-swap '(1 2 3 4 5) 1 5)]}
  
@defproc[(list-delimit [lis list?] [delim any]) list?]{

Inserts @scheme[delim] between each pair of consecutive items in @scheme[lis]. Useful for assembling x-expressions in web applications.

@examples[
  #:eval list-eval
  (list-delimit '(1 2 3 4 5) ", ")]}

@defproc[(list-pad [lis list?] [target-length natural?] [item any #f]) list?]{

Creates a new list by repeatedly adds @scheme[item] to the left of @scheme[lis] until the @scheme[target-length] is reached. @scheme[lis] is returned if it is already @scheme[target-length] or longer.

@examples[
  #:eval list-eval
  (list-pad '(1 2 3) 5)
  (list-pad '(1 2 3) 5 #t)
  (list-pad '(1 2 3 4 5) 3)]}

@defproc[(list-pad-right [lis list?] [target-length natural?] [item any #f]) list?]{

Creates a new list by repeatedly adds @scheme[item] to the right of @scheme[lis] until the @scheme[target-length] is reached. @scheme[lis] is returned if it is already @scheme[target-length] or longer.

@examples[
  #:eval list-eval
  (list-pad-right '(1 2 3) 5)
  (list-pad-right '(1 2 3) 5 #t)
  (list-pad-right '(1 2 3 4 5) 3)]}

@defproc[(merge-sorted-lists [list1 list?]
                             [list2 list?]
                             [same? (any any -> boolean?)]
                             [less-than? (any any -> boolean?)]) list?]{
                            
Merges @scheme[list1] and @scheme[list2] in @tt{O(n)} time. The result is a sorted list of items from both lists, with all duplicates removed.

Duplicates are detected using the supplied predicate @scheme[same?]. Items are taken from list1 when duplicates are detected.
 
The procedure assumes @scheme[list1] and @scheme[list2] are sorted in ascending order according to the supplied predicate @scheme[less-than?]. More formally, for each pair of consecutive items @scheme[a] and @scheme[b] in either list, @scheme[(less-than? a b)] must be @scheme[#t].

@examples[
  #:eval list-eval
  (merge-sorted-lists '(1 3 5) '(2 4) = <)
  (merge-sorted-lists '("a" "b" "c") '("b" "c" "d") 
                      string=? string<?)
  (merge-sorted-lists '("a" "b" "c") '("b" "c" "d") eq? string<?)
  (code:comment "Sorted in the wrong order: results undefined:")
  (merge-sorted-lists '(1 3 5) '(4 2) = <)]}

@defproc[(char-iota [count integer?] [start char? #\a] [step positive-integer? 1]) (listof char?)]{

The character equivalent of @scheme[iota] from SRFI 1.

@examples[
  #:eval list-eval
  (char-iota 5)
  (char-iota 5 #\V)
  (char-iota 5 #\a 2)]}

@section{Association lists}

@defproc[(assoc-value [key any] [alist (listof pair?)]) any]{

Like @scheme[assoc] but returns the @scheme[cdr] of the relevant pair. Raises @scheme[exn:fail] if @scheme[key] is not found.

@examples[
  #:eval list-eval
  (assoc-value 'a '((a . 1) (b . 2)))
  (assoc-value 'b '((a . 1) (b . 2)))
  (assoc-value 'c '((a . 1) (b . 2)))]}

@defproc[(assoc-value/default [key any] [alist (listof pair?)] [default any]) any]{

Like @scheme[assoc] but returns the @scheme[cdr] of the relevant pair. Returns @scheme[default] if @scheme[key] is not found.

@examples[
  #:eval list-eval
  (assoc-value/default 'a '((a . 1) (b . 2)) #f)
  (assoc-value/default 'b '((a . 1) (b . 2)) #f)
  (assoc-value/default 'c '((a . 1) (b . 2)) #f)]}
  
@defproc[(alist-set [key any] [val any] [alist (listof pair?)]) (listof pair?)]{

Returns a copy of @scheme[alist] with the value mapped to @scheme[key] replaced with @scheme[val]. If @scheme[key] does not appear in @scheme[alist], a new pair is added to the end. The order of existing keys in @scheme[alist] is preserved.

@examples[
  #:eval list-eval
  (alist-set 'a 5 '((a . 1) (b . 2)))
  (alist-set 'c 5 '((a . 1) (b . 2)))]}

@defproc[(alist-delete [key any] [alist (listof pair?)]) (listof pair?)]{

Reprovided from SRFI 1. Returns a copy of @scheme[alist] with @scheme[key] removed.

@examples[
  #:eval list-eval
  (alist-delete 'a '((a . 1) (b . 2)))
  (alist-delete 'c '((a . 1) (b . 2)))]}

@defproc[(alist-map [proc (key val -> ans)] [alist (listof (cons key val))]) (listof ans)]{

Maps @scheme[proc] over @scheme[alist], returning a list of the results.

@examples[
  #:eval list-eval
  (alist-map + '((1 . 2) (3 . 4) (5 . 6)))]}

@defproc[(alist-for-each [proc (key val -> void?)] [alist (listof (cons key val))]) void?]{

Applies @scheme[proc] to each item in @scheme[alist] for its side effects.

@examples[
  #:eval list-eval
  (define (print-pair key val)
    (printf "~a = ~a, " key val))
  (alist-for-each print-pair '((1 . 2) (3 . 4) (5 . 6)))]}

@defproc[(alist-merge [alist1 (listof pair?)]
                      [alist2 (listof pair?)]
                      [prefer (U 'first 'second) 'first]) (alistof pair?)]{

Merges @scheme[alist1] and @scheme[alist2], merging colliding keys together. @scheme[prefer] determines which alist the value is taken from if keys collide. @scheme[equal?] is used as a key comparison function. The order of keys is preserved where possible.

@examples[
  #:eval list-eval
  (alist-merge '((a . 1) (b . 2) (c . 3))
               '((b . 20) (c . 30) (d . 40)))
  (alist-merge '((a . 1) (b . 2) (c . 3))
               '((b . 20) (c . 30) (d . 40))
               'second)]}
