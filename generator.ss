#lang scheme/base

(require (for-syntax scheme/base
                     "base.ss")
         scheme/dict
         "base.ss"
         "debug.ss"
         (only-in "project.ss" partition/mask)
         (only-in "yield.ss" yieldable))

; There is no doubt that lists are useful structures for representing
; many kinds of data, and that folds and maps are a quick, convenient
; way of performing arbitrary bits of list manipulation.
;
; The main problem with the list/fold/map approach is the number of
; temporary lists generated in the process, which can take up a large
; amount of memory.
; 
; Generators are a half-way-house between lists and streams that aim
; to reduce memory overhead when large data sources are involved.
;
; A generator is a stream-like accessor that can be repeatedly called
; to return new values from its source. A special "generator-end" value
; is returned to indicate that the source has been exhausted.
;
; For convenience we write a generator of a type "a" as follows:
;
;     (gen-> a) === (-> (U a generator-end))
;
; This library provides convenient ways of:
;
;   - producing generators from lists
;   - combining generators to form other generators 
;     (c.f. fold, map and so on)
;   - accumulating results from generators
;     (e.g. back into lists)

; Variables ------------------------------------

; symbol
(define generator-end (gensym 'generator-end))

; Syntax ---------------------------------------

; (_ flat-contract) -> flat-contract
;
; Expands into a contract that works with values and the generator-end symbol.
(define-syntax gen->
  (syntax-rules ()
    [(_ expr) 
     (-> (or/c expr generator-end?))]))

; Core procedures ------------------------------

; any -> boolean
(define (generator-end? item)
  (eq? item generator-end))

; (listof (gen-> any)) -> (listof any)
(define (generate-all gens)
  (map (lambda (item)
         (item))
       gens))

; Combinators ----------------------------------

; (a b c ... -> d) (gen-> a) (gen-> b) (gen-> c) ... -> (gen-> d)
;
; The generator equivalent of "map" from SRFI 1.
;
; Given a mapping function "fn" and some sources, creates a generator that returns:
; 
;     (apply fn sources)
;
; If, in a given iteration, any of the sources return generator-end, the mapping
; function is not called, and the generator simply returns generator-end.
(define (generator-map fn . gens)
  (let ([id (gensym)])
    (lambda ()
      (let ([args (generate-all gens)])
        (if (ormap generator-end? args)
            generator-end
            (apply fn args))))))

; (a b c ... k -> k) k (gen-> a) (gen-> b) (gen-> c) ... -> (gen-> k)
;
; One generator equivalent of "fold" from SRFI 1.
;
; Given an iterator function "it", an initial accumulator and some sources,
; creates a generator that returns:
;
;     (apply it (append sources (list accum)).
;
; The result is stored after each iteration and used as the accumulator for the
; next iteration.
;
; If, in a given iteration, any of the sources return generator-end, the iterator
; function is not called, and the generator simply returns generator-end.
(define (generator-fold-map proc accum . gens)
  (lambda ()
    (let ([args (generate-all gens)])
      (if (ormap generator-end? args)
          generator-end
          (begin
            ; Update the accumulator...
            (set! accum (apply proc (append args (list accum))))
            ; ...and return it.
            accum)))))

; (a -> boolean) (gen-> a) -> (gen-> a)
;
; The generator equivalent of "filter" from SRFI 1.
;
; Given a predicate "pred" and a source, creates a generator that returns
; only those source values for which:
;
;     (pred source)
;
; is non-#f. Note that this means that a single call to the generator can result
; in multiple calls to the source.
;
; If, in a given iteration, the source returns generator-end, the iterator
; function is not called, and the generator simply returns generator-end.
(define (generator-filter test gen)
  (letrec ([ans (lambda ()
                  (let ([arg (gen)])
                    (cond [(generator-end? arg) generator-end]
                          [(test arg)           arg]
                          [else                 (ans)])))])
    ans))

; (a -> (U any #f)) (gen-> a) -> (gen-> any)
;
; The generator equivalent of "filter-map" from SRFI 1.
;
; Given a predicate "pred" and a source, creates a generator that returns non-#f
; values of:
;
;     (pred source)
;
; Note that this means that a single call to the generator can result in 
; multiple calls to the source.
;
; If, in a given iteration, the source returns generator-end, the iterator
; function is not called, and the generator simply returns generator-end.
(define (generator-filter-map test gen)
  (letrec ([ans (lambda ()
                  (let ([arg (gen)])
                    (if (generator-end? arg)
                        generator-end
                        (let ([answer (test arg)])
                          (if answer answer (ans))))))])
    ans))

; (gen-> a) [(a a -> boolean)] -> (gen-> a)
(define generator-remove-duplicates
  (let ([empty (gensym)])
    (lambda (gen [same? equal?])
      (let ([last empty])
        (lambda ()
          (let loop ([curr (gen)])
            (cond [(generator-end? curr) generator-end]
                  [(same? last curr)     (set! last curr)
                                         (loop (gen))]
                  [else                  (set! last curr)
                                         curr])))))))

; string (gen-> any) -> (gen-> any)
;
; Creates a generator that mimics its source, but prints generated values
; as it goes.
(define (generator-debug message generate)
  (lambda ()
    (let ([item (generate)])
      (printf "~a ~s~n" message item)
      item)))

; Accumulators and list interoperability -------

; (a b c ... -> void) (gen-> a) (gen-> b) (gen-> c) ... -> void
;
; Repeatedly calls source generators, supplying their values to an iterator
; procedure, until one or more returns generator-end.
(define (generator-for-each proc . gens)
  (let ([args (generate-all gens)])
    (if (ormap generator-end? args)
        (void)
        (begin (apply proc args)
               (apply generator-for-each (cons proc gens))))))

; (a b c ... k -> k) k (gen-> a) (gen-> b) (gen-> c) ... -> k
;
; The "proper" equivalent of "fold" from SRFI 1.
; 
; Given an iterator function "it", an initial accumulator and some sources,
; repeatedly does:
;
;     (apply it (append sources (list accum))
;
; until one or more of the sources returns generator-end. At this point the
; accumulator is returned.
(define (generator-fold proc accum0 . gens)
  (let loop ([accum accum0])
    (let ([args (generate-all gens)])
      (if (ormap generator-end? args)
          accum
          (loop (apply proc (append args (list accum))))))))

; (listof a) -> (-> (U a generator-end))
;
; Creates a generator that iterates through the values in data and then
; repeatedly returns end.
(define (list->generator data)
  (lambda ()
    (if (null? data)
        generator-end
        (begin0 (car data)
                (set! data (cdr data))))))

; integer [(U integer #f)] [integer] -> (gen-> integer)
(define (range->generator start [end #f] [step 1])
  ; integer
  (define counter start)
  (lambda ()
    (cond [(not end) (begin0 counter (set! counter (+ counter step)))]
          [(and (> step 0) (>= counter end)) generator-end]
          [(and (< step 0) (<= counter end)) generator-end]
          [else (begin0 counter (set! counter (+ counter step)))])))

; (gen-> a) -> (listof a)
;
; A convenient form of generator-fold that collects generated values
; into a list.
(define (generator->list gen)
  (reverse (generator-fold cons null gen)))

;  (gen-> a)
;  (a -> b)
;  [(a -> c)]
;  [#:initial-hash (hashof b c)]
; ->
;  (hashof b c)
(define (generator->hash gen item->key [item->val (lambda (x) x)] [hash (make-hash)])
  (generator-for-each (lambda (item)
                        (hash-set! hash 
                                   (item->key item)
                                   (item->val item)))
                      gen)
  hash)

; Snooze specific (TODO : move to Snooze) ------

;  (listof boolean)
;  (gen-> (listof a))
;  [(a a -> boolean)]
; ->
;  (gen-> projected)
;
; where projected : (append (listof a) (listof (listof a)))
;
; Projects items from the supplied generator according to the rules
; set out in project.ss.
;
; Passes non-list items straight through.
(define (generator-project mask generate [same? eq?])
  ; any -> boolean
  (define (projectable? x)
    (or (pair? x) (null? x)))
  ; (listof a)
  (define last (generate))
  ; (listof a)
  ; (listof (listof a))
  (define-values (last-keys nonkeys-accum)
    (if (list? last)
        (let-values ([(last-keys last-nonkeys)
                      (partition/mask last mask)])
          (values last-keys (list last-nonkeys)))
        (values #f null)))
  ; (listof a) (listof (listof a)) -> projected
  (define (make-answer keys nonkeys)
    (append keys (list (reverse nonkeys))))
  ; (gen-> projected)
  (define (loop)
    (define next (generate))
    (define-values (next-keys next-nonkeys) 
      (if (projectable? next)
          (partition/mask next mask)
          (values #f null)))
    (if (projectable? last)
        (if (projectable? next)
            (if (andmap same? last-keys next-keys)
                (begin (set! last next)
                       (set! last-keys next-keys)
                       (set! nonkeys-accum (cons next-nonkeys nonkeys-accum))
                       (loop))
                (begin0 (make-answer last-keys nonkeys-accum)
                        (set! last next)
                        (set! last-keys next-keys)
                        (set! nonkeys-accum (list next-nonkeys))))
            (begin0 (make-answer last-keys nonkeys-accum)
                    (set! last next)
                    (set! last-keys #f)
                    (set! nonkeys-accum null)))
        (if (projectable? next)
            (begin0 last
                    (set! last next)
                    (set! last-keys next-keys)
                    (set! nonkeys-accum (list next-nonkeys)))
            (begin0 last
                    (set! last next)
                    (set! last-keys #f)
                    (set! nonkeys-accum null)))))
  loop)


; generator -> sequence
(define (in-generator g:items)
  (make-do-sequence
   ; current-position tracks the last value of the generator
   (let ([current-position (g:items)])
     (lambda ()
       (values (lambda (pos)
                 current-position)
               (lambda (pos)
                 (set! current-position (g:items))
                 #t)
               #t    ; position is irrelevant, so is always #t
               (lambda (pos)
                 (not (generator-end? current-position)))
               (lambda (val)
                 (not (generator-end? val)))
               (lambda (pos val)
                 (or (not (generator-end? current-position))
                     (not (generator-end? val)))))))))

; Provide statements ---------------------------

(provide gen->
         generator-end 
         generator-end?)

(provide/contract
 [generator-map               (->* (procedure?) () #:rest (listof procedure?) procedure?)]
 [generator-fold-map          (->* (procedure? any/c) () #:rest (listof procedure?) procedure?)]
 [generator-filter            (-> procedure? procedure? procedure?)]
 [generator-filter-map        (-> procedure? procedure? procedure?)]
 [generator-remove-duplicates (->* (procedure?) (procedure?) procedure?)]
 [generator-debug             (-> string? procedure? procedure?)]
 [generator-for-each          (->* (procedure?) () #:rest (listof procedure?) any)]
 [generator-fold              (->* (procedure? any/c) () #:rest (listof procedure?) any)]
 [generator->list             (-> procedure? (or/c pair? null?))]
 [generator->hash             (->* (procedure? procedure?) 
                                   (procedure? (and/c hash? dict-mutable?))
                                   (and/c hash? dict-mutable?))]
 [list->generator             (-> (or/c pair? null?) procedure?)]
 [range->generator            (->* (integer?) ((or/c integer? false/c) integer?) procedure?)]
 [generator-project           (->* ((listof boolean?) procedure?) (procedure?) procedure?)]
 [in-generator                (-> (gen-> any/c) sequence?)]) 
