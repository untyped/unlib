(module list mzscheme
  
  (require (lib "contract.ss")
           (lib "etc.ss")
           (only (lib "list.ss" "srfi" "1") make-list fold take drop alist-delete)
           (lib "cut.ss" "srfi" "26") 
           (file "base.ss")
           (file "debug.ss"))
  
  ;; quick-list/c : contract
  ;;
  ;; list/c is too labour intensive for the procedures here because
  ;; it scans the whole structure to check for circular and improper
  ;; lists.
  ;;
  ;; This contract is a quick-and-dirty way of checking that
  ;; an argument is *likely* to be a proper list without going through
  ;; all that fuss.
  (define quick-list/c
    (or/c pair? null?))
  
  ;; tree/c : contract
  ;;
  ;; A tree is one of the following:
  ;;   - an atom
  ;;   - (cons tree tree)
  ;;
  ;; So technically a tree can be anything.
  (define tree/c any/c)
  
  ;; Syntax from this file:
  (provide alist-accessor
           alist-accessor/default
           alist-mutator
           alist-mutator/append)
  
  ;; Procedures from this file:
  ;;
  ;; These contracts should all be fairly light-weight. We don't want
  ;; anything non-tail-recursive or too labour intensive (always use
  ;; the "any" return value syntax).
  (provide/contract
   [mutable-cons        (-> any/c any/c any)]
   [list-swap           (-> quick-list/c any/c any/c any)]
   [list-delimit        (-> quick-list/c any/c any)]
   [merge-sorted-lists  (-> quick-list/c quick-list/c procedure? procedure? any)]
   [char-iota           (opt-> (integer?) (char?) any)]
   [list-pad            (opt-> (quick-list/c integer?) (any/c) any)]
   [list-pad-right      (opt-> (quick-list/c integer?) (any/c) any)]
   [tree-map            (-> procedure? tree/c any)]
   [tree-for-each       (-> procedure? tree/c any)]
   [assoc-value         (-> any/c quick-list/c any)]
   [assoc-value/default (-> any/c quick-list/c any/c any)]
   [alist-set           (-> any/c any/c quick-list/c any)]
   [alist-map           (-> procedure? quick-list/c any)]
   [alist-for-each      (-> procedure? quick-list/c any)]
   [alist-merge         (opt-> (quick-list/c quick-list/c) ((symbols 'first 'second)) any)])
   
  ;; From SRFI 1:
  (provide alist-delete) ; alist-delete : any1 (alist-of any1 any2) -> (alist-of any1 any2)
  
  ;; mutable-cons : a b -> (mutable-pair a b)
  (define mutable-cons cons)
  
  ;; list-swap : list integer integer -> list
  (define (list-swap data index1 index2)
    (cond [(< index2 index1)
           (list-swap data index2 index1)]
          [(= index1 index2)
           (raise-exn exn:fail:contract
             (format "List indices must be differnet: ~a ~a" index1 index2))]
          [(or (< index2 0) (> index1 (length data)))
           (raise-exn exn:fail:contract
             (format "List indices out of bounds: ~a ~a" index1 index2))]
          [else
           (let ([item1    (list-ref data index1)]
                 [item2    (list-ref data index2)]
                 [slice0-1 (take data index1)]
                 [slice1-2 (take (drop data (add1 index1)) (sub1 (- index2 index1)))]
                 [slice2-3 (drop data (add1 index2))])
             (append slice0-1
                     (cons item2 slice1-2) 
                     (cons item1 slice2-3)))]))
  
  ;; list-delimit : (list-of a) b -> (cons a (cons b (cons a ... (list b))))
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
  
  ;; merge-sorted-lists 
  ;;     : (list-of any1)
  ;;       (list-of any2) (any1 any2 -> boolean) (any1 any2 -> boolean) -> (list-of (U any1 any2))
  ;;
  ;; Merges list1 and list2 in O(n) time. The result is a sorted list
  ;; of items from both lists, with all duplicates removed.
  ;;
  ;; Duplicates are detected using the supplied predicate same?. Items
  ;; are taken from list1 when duplicates are detected (this is useful to
  ;; know if same? returns #t for two items that are only similar).
  ;; 
  ;; The procedure assumes list1 and list2 are sorted in ascending order
  ;; according to the supplied predicate less-than?. More formally, for
  ;; each pair of adjacent items a and b in each list, the following
  ;; expression holds:
  ;;
  ;;   (or (same? a b) (less-than? a b))
  (define (merge-sorted-lists list1 list2 same? less-than?)
    ; swallow : any (list-of any) -> (list-of any)
    ;
    ; Removes duplicates from the beginning of a list.
    (define (swallow item list)
      (cond [(null? list)            list]
            [(same? item (car list)) (swallow item (cdr list))]
            [else                    list]))
    ; merge : (list-of any1) (list-of any2) -> (list-of (U any1 any2))
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
  
  ;; char-iota : integer [char] -> (list-of char)
  (define char-iota
    (opt-lambda (count [start #\a])
      (let loop ([i 0] [curr (char->integer start)])
        (if (< i count)
            (cons (integer->char curr)
                  (loop (add1 i) (add1 curr)))
            null))))
  
  ;; list-pad : (list-of any) integer [any] -> (list-of any)
  (define list-pad
    (opt-lambda (lis target-length [item #f])
      (let loop ([current-length (length lis)] [accum lis])
        (if (< current-length target-length)
            (loop (add1 current-length) (cons item accum))
            accum))))
  
  ;; list-pad-right : (list-of any) integer [any] -> (list-of any)
  (define list-pad-right
    (opt-lambda (lis target-length [item #f])
      (reverse (list-pad (reverse lis) target-length item))))
  
  ; Tree iterators -------------------------------
  
  ;; tree-map : (a -> b) (tree-of a) -> (tree-of b)
  (define (tree-map fn tree)
    (let loop ([item tree])
      (cond
        [(list? item) (map loop item)]
        [(pair? item) (cons (loop (car item)) (loop (cdr item)))]
        [else (fn item)])))
  
  ;; tree-for-each : (a -> any) (tree-of a) -> void
  (define (tree-for-each fn tree)
    (let loop ([item tree])
      (cond
        [(list? item) 
         (for-each loop item)]
        [(pair? item)
         (loop (car item))
         (loop (cdr item))]
        [else 
         (fn item)])))
  
  ; Association list utilities -------------------
  
  ;; assoc-value : any1 (list-of (cons any1 any2)) -> any2 
  ;; 
  ;; Searches for a value by key in a list of key/value pairs 
  ;; (an association list). If the key is not found, an exception
  ;; of type exn:fail:unlib is raised.
  (define (assoc-value key alist)
    (let ([kvp (assoc key alist)])
      (if kvp
          (cdr kvp)
          (raise-exn exn:fail:unlib
            (format "Key ~a not found in ~a.~n" key alist)))))
  
  ;; assoc-value/default : any1 (list-of (cons any1 any2)) any2 -> any2 
  ;; 
  ;; Searches for a value by key in a list of key/value pairs
  ;; (an association list). If the key is not found, the default
  ;; value is returned instead.
  (define (assoc-value/default key alist default)
    (let ([kvp (assoc key alist)])
      (if kvp
          (cdr kvp)
          default)))

  ;; syntax alist-accessor : (list-of (cons any1 any2)) -> (any1 -> any2)
  ;;
  ;; Creates a procedure that can be used to retrieve a
  ;; value from an association list.
  ;;
  ;; See also: hash-table-accessor in hash-table.ss.
  (define-syntax (alist-accessor stx)
    (syntax-case stx ()
      [(_ alist)
       #'(lambda (key)
           (assoc-value key alist))]))
  
  ;; syntax alist-accessor/default : (list-of (cons any1 any2)) any2 -> (any1 -> any2)
  ;;
  ;; Creates a procedure that can be used to retrieve a
  ;; value from an association list.
  ;;
  ;; See also: hash-table-accessor/default in hash-table.ss.
  (define-syntax (alist-accessor/default stx)
    (syntax-case stx ()
      [(_ alist default)
       #'(lambda (key)
           (assoc-value/default key alist default))]))
  
  ;; alist-set : any1 any2 (list-of (cons any1 any2)) -> (list-of (cons any1 any2)) 
  ;;
  ;; Sets the value of key in alist. If key is not already in alist,
  ;; a new key/value pair is added to the end. The new list is returned
  (define (alist-set key value alist)
    (let* ([found #f]
           [new-alist
            (map
             (lambda (kvp)
               (if (equal? key (car kvp))
                   (begin
                     (set! found #t)
                     (cons (car kvp) value))
                   kvp))
             alist)])
      (if found
          new-alist
          (append new-alist
                  (list (cons key value))))))
  
  ;; syntax alist-mutator : (list-of (cons any1 any2)) -> (any1 any2 -> nothing)
  ;;
  ;; Creates a procedure that can be used to update alist.
  ;;
  ;; See also: hash-table-mutator in hash-table.ss.
  (define-syntax (alist-mutator stx)
    (syntax-case stx ()
      [(_ alist)
       #'(lambda (key val)
           (set! alist (alist-set key val alist)))]))
  
  ;; syntax alist-mutator/append : (list-of (cons any1 (list-of any2))) -> (any1 any2 -> nothing)
  ;;
  ;; Creates a procedure that can be used to update alist.
  ;; Rather than overwriting the values mapped to keys in the
  ;; list, the mutator appends new values to the end of existing
  ;; ones.
  ;;
  ;; See also: hash-table-mutator/append in hash-table.ss.
  (define-syntax (alist-mutator/append stx)
    (syntax-case stx ()
      [(_ alist)
       #'(lambda (key val)
           (let ([curr (assoc-value/default key alist null)])
             (set! alist (alist-set key (append curr (list val)) alist))))]))
  
  ;; alist-map : (any1 any2 -> any3) (list-of (cons any1 any2)) -> (list-of any3)
  ;;
  ;; Applies proc to each pair in alist. Proc must accept two arguments: 
  ;; a key and a value. If any element of alist is not a pair, an exception
  ;; is thrown. A list of the results of proc is returned.
  (define (alist-map proc alist)
    (map
     (lambda (kvp)
       (if (pair? kvp)
           (proc (car kvp) (cdr kvp))
           (raise-exn
               exn:fail:unlib
             (format "alist-map: expected a pair: ~a" kvp))))
     alist))
  
  ;; alist-for-each : (any1 any2 -> any3) (list-of (cons any1 any2)) -> void
  ;;
  ;; Applies proc to each pair in alist for its side effects. Proc must accept 
  ;; two arguments: a key and a value. If any element of alist is not a pair,
  ;; an exception is thrown.
  (define (alist-for-each proc alist)
    (for-each
     (lambda (kvp)
       (if (pair? kvp)
           (proc (car kvp) (cdr kvp))
           (raise-exn
               exn:fail:unlib
             (format "alist-for-each: expected a pair: ~a" kvp))))
     alist))
  
  ;; alist-merge : (alist-of a b) (alist-of c d) -> (alist-of (U a c) (U b d))
  ;;
  ;; Merges two alists by appending keys from the second list to the end of the first.
  ;; 
  ;; The optional "prefer" argument states which list to prefer if keys collide.
  ;; The default is list1.
  (define alist-merge
    (opt-lambda (list1 list2 [prefer 'first])
          ; This fold accumulates items from list2 into a cons of two lists:
          ;   - X - a copy of list1 with duplicate keys overwritten
          ;   - Y - a list of pairs to add to the end of X to complete the merge
          ; Y is accumulated in reverse order, so it has to reversed before it is appended.
      (define proc
        (if (eq? prefer 'first)
            (lambda (item accum)
              (let ([key (car item)]
                    [x (car accum)]
                    [y (cdr accum)])
                (if (assoc key x)
                    (cons x y)
                    (cons x (cons item y)))))
            (lambda (item accum)
              (let ([key (car item)]
                    [x (car accum)]
                    [y (cdr accum)])
                (if (assoc key x)
                    (cons (alist-set key (cdr item) x) y)
                    (cons x (cons item y)))))))
      (let ([x-and-y (fold proc (cons list1 null) list2)])
        (append (car x-and-y) (reverse (cdr x-and-y))))))
  
  )