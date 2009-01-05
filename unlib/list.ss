#lang scheme/base

(require (for-syntax scheme/base)
         (only-in srfi/1 make-list fold take drop alist-delete)
         "base.ss"
         "contract.ss"
         "debug.ss"
         "number.ss")

; natural (cons any (listof any)) -> (listof any)
(define (make-list* num items)
  ; natural natural
  (define-values (num-complete num-remaining)
    (quotient/remainder num (length items)))
  ; natural natural list -> list
  (let loop ([num-complete num-complete] [num-remaining num-remaining] [items items])
    (if (zero? num-complete)
        (take items num-remaining)
        (append items (loop (sub1 num-complete) num-remaining items)))))

; (_ [expr any ...] ...) -> (listof any)
(define-syntax (assemble-list stx)
  
  ; syntax -> (listof syntax)
  (define (expand-clause clause-stx)
    (syntax-case clause-stx (unquote-splicing)
      [(#t (unquote-splicing items))   (list #',@items)]
      [(#f (unquote-splicing items))   null]
      [(expr (unquote-splicing items)) (list #`,@(if expr items null))]
      [(#t item ...)                   (syntax->list #'((unquote item) ...))]
      [(#f item ...)                   null]
      [(expr item ...)                 (list #`,@(if expr (list item ...) null))]))
  
  ; (listof syntax) -> (listof syntax)
  (define (expand-clauses clause-stxs)
    (if (null? clause-stxs)
        null
        (let ([curr (car clause-stxs)]
              [rest (cdr clause-stxs)])
          (append (expand-clause curr)
                  (expand-clauses rest)))))
  
  (syntax-case stx ()
    [(_ clause ...)
     #``(#,@(expand-clauses (syntax->list #'(clause ...))))]))

; (cons any list) -> sequence
(define (in-list/cycle items)
  (make-do-sequence
   (lambda ()
     (values (lambda (pos)
               (car pos))
             (lambda (pos)
               (if (null? (cdr pos))
                   items
                   (cdr pos)))
             items
             (lambda (pos)
               #t)
             (lambda (val)
               #t)
             (lambda (pos val)
               #t)))))

; list integer integer -> list
(define (list-swap data index1 index2)
  (cond [(< index2 index1)
         (list-swap data index2 index1)]
        [(= index1 index2)
         (raise-exn exn:fail:contract
           (format "List indices must be differnet: ~a ~a" index1 index2))]
        [(or (< index2 0) (> index1 (length data)))
         (raise-exn exn:fail:contract
           (format "List indices out of bounds: ~a ~a" index1 index2))]
        [else (let ([item1    (list-ref data index1)]
                    [item2    (list-ref data index2)]
                    [slice0-1 (take data index1)]
                    [slice1-2 (take (drop data (add1 index1)) (sub1 (- index2 index1)))]
                    [slice2-3 (drop data (add1 index2))])
                (append slice0-1
                        (cons item2 slice1-2) 
                        (cons item1 slice2-3)))]))

; (listof a) b -> (cons a (cons b (cons a ... (list b))))
(define (list-delimit list delimiter)
  (if (null? list)
      null
      (let loop ([rest list])
        (if (null? (cdr rest))
            (cons (car rest) 
                  null)
            (cons (car rest) 
                  (cons delimiter
                        (loop (cdr rest))))))))

; (listof any) integer [any] -> (listof any)
(define (list-pad lis target-length [item #f])
  (let loop ([current-length (length lis)] [accum lis])
    (if (< current-length target-length)
        (loop (add1 current-length) (cons item accum))
        accum)))

; (listof any) integer [any] -> (listof any)
(define (list-pad-right lis target-length [item #f])
  (reverse (list-pad (reverse lis) target-length item)))

;  (listof any1)
;  (listof any2)
;  (any1 any2 -> boolean)
;  (any1 any2 -> boolean)
; ->
;  (listof (U any1 any2))
;
; Merges list1 and list2 in O(n) time. The result is a sorted list
; of items from both lists, with all duplicates removed.
;
; Duplicates are detected using the supplied predicate same?. Items
; are taken from list1 when duplicates are detected (this is useful to
; know if same? returns #t for two items that are only similar).
; 
; The procedure assumes list1 and list2 are sorted in ascending order
; according to the supplied predicate less-than?. More formally, for
; each pair of adjacent items a and b in each list, the following
; expression holds:
;
;   (or (same? a b) (less-than? a b))
(define (merge-sorted-lists list1 list2 same? less-than?)
  ; any (listof any) -> (listof any)
  ;
  ; Removes duplicates from the beginning of a list.
  (define (swallow item list)
    (cond [(null? list)            list]
          [(same? item (car list)) (swallow item (cdr list))]
          [else                    list]))
  ; (listof any1) (listof any2) -> (listof (U any1 any2))
  ;
  ; Merges two lists.
  (define (merge list1 list2)
    (cond [(null? list1) list2]
          [(null? list2) list1]
          [else (let ([head1 (car list1)]
                      [head2 (car list2)])
                  (cond [(same? head1 head2)
                         (cons head1 (merge (swallow head1 list1)
                                            (swallow head2 list2)))]
                        [(less-than? head1 head2)
                         (cons head1 (merge (swallow head1 list1) list2))]
                        [else 
                         (cons head2 (merge list1 (swallow head2 list2)))]))]))
  ; Main procedure body:
  (merge list1 list2))

; integer [char] -> (listof char)
(define (char-iota count [start #\a] [step 1])
  (let loop ([i 0] [curr (char->integer start)])
    (if (< i count)
        (cons (integer->char curr)
              (loop (add1 i) (+ curr step)))
        null)))

; iterator -> list ...
(define (unzip-values input)
  (define accum
    (for/fold ([accum (void)])
              ([item input])
              (if (void? accum)
                  (map cons item (make-list (length item) null))
                  (map cons item accum))))
  (apply values (map reverse accum)))

; Association lists ------------------------------

; any1 (listof (cons any1 any2)) -> any2 
; 
; Searches for a value by key in a list of key/value pairs (an association list). 
; exn:fail is raised if the key is not found.
(define (assoc-value key alist)
  (let ([kvp (assoc key alist)])
    (if kvp
        (cdr kvp)
        (error "assoc-value: key not found:" key alist))))

; any1 (listof (cons any1 any2)) any2 -> any2 
; 
; Searches for a value by key in a list of key/value pairs
; (an association list). If the key is not found, the default
; value is returned instead.
(define (assoc-value/default key alist default)
  (define kvp (assoc key alist))
  (if kvp
      (cdr kvp)
      default))

; a b (alistof a b) [(a b -> boolean)] -> (alistof a b)
;
; Sets the value of key in alist. If key is not already in alist,
; a new key/value pair is added to the end. The new list is returned
(define (alist-set key val alist [same? equal?])
  (define found #f)
  (define new-alist
    (alist-map (lambda (key1 val1)
                 (if (same? key key1)
                     (begin (set! found #t)
                            (cons key1 val))
                     (begin (cons key1 val1))))
               alist))
  (if found
      new-alist
      (append new-alist
              (list (cons key val)))))

; (any1 any2 -> any3) (listof (cons any1 any2)) -> (listof any3)
;
; Applies proc to each pair in alist. Proc must accept two arguments: 
; a key and a value. If any element of alist is not a pair, an exception
; is thrown. A list of the results of proc is returned.
(define (alist-map proc alist)
  (map (match-lambda 
         [(list-rest key val)
          (proc key val)]
         [other (raise-exn exn:fail:contract 
                  (format "Expected (listof pair), recevied ~s" alist))])
       alist))

; (any1 any2 -> void) (listof (cons any1 any2)) -> void
;
; Applies proc to each pair in alist for its side effects. Proc must accept 
; two arguments: a key and a value. If any element of alist is not a pair,
; an exception is thrown.
(define (alist-for-each proc alist)
  (for-each (match-lambda 
              [(list-rest key val)
               (proc key val)]
              [other (raise-exn exn:fail:contract 
                       (format "Expected (listof pair), recevied ~s" alist))])
            alist))

; (alistof a b) (alistof c d) [(U 'first 'second)] -> (alistof (U a c) (U b d))
;
; Merges two alists by appending keys from the second list to the end of the first.
; 
; The optional "prefer" argument states which list to prefer if keys collide.
; The default is list1.
(define (alist-merge list1 list2 [prefer 'first] [find assoc])
  ; This fold accumulates items from list2 into a cons of two lists:
  ;   - X - a copy of list1 with duplicate keys overwritten
  ;   - Y - a list of pairs to add to the end of X to complete the merge
  ; Y is accumulated in reverse order, so it has to reversed before it is appended.
  (define proc
    (if (eq? prefer 'first)
        (lambda (item accum)
          (let ([key (car item)]
                [x   (car accum)]
                [y   (cdr accum)])
            (if (find key x)
                (cons x y)
                (cons x (cons item y)))))
        (lambda (item accum)
          (let ([key (car item)]
                [x   (car accum)]
                [y   (cdr accum)])
            (if (find key x)
                (cons (alist-set key (cdr item) x) y)
                (cons x (cons item y)))))))
  (let ([x-and-y (fold proc (cons list1 null) list2)])
    (append (car x-and-y) (reverse (cdr x-and-y)))))

; Provide statements -----------------------------

; contract
;
; Quick-and-dirty version of list/c.
(define qlist/c (or/c pair? null?))

; Re-provided from SRFI 1:
(provide assemble-list
         alist-delete) ; any1 (alistof any1 any2) -> (alistof any1 any2)

(provide/contract
 [make-list*          (-> natural? (cons/c any/c list?) list?)]
 [in-list/cycle       (-> (cons/c any/c list?) sequence?)]
 [list-swap           (-> qlist/c any/c any/c any)]
 [list-delimit        (-> qlist/c any/c any)]
 [merge-sorted-lists  (-> qlist/c qlist/c (arity/c 2) (arity/c 2) any)]
 [char-iota           (->* (integer?) (char? integer?) any)]
 [unzip-values        (-> sequence? any)]
 [list-pad            (->* (qlist/c integer?) (any/c) any)]
 [list-pad-right      (->* (qlist/c integer?) (any/c) any)]
 [assoc-value         (-> any/c qlist/c any)]
 [assoc-value/default (-> any/c qlist/c any/c any)]
 [alist-set           (->* (any/c any/c qlist/c) (procedure?) any)]
 [alist-map           (-> (arity/c 2) qlist/c any)]
 [alist-for-each      (-> (arity/c 2) qlist/c any)]
 [alist-merge         (->* (qlist/c qlist/c) ((symbols 'first 'second)) any)])
