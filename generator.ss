(module generator mzscheme
  
  (require-for-syntax
   (lib "contract.ss"))
  
  (require (lib "contract.ss")
           (lib "etc.ss")
           (only (file "project.ss") partition/mask)
           (only (file "yield.ss") yieldable)
           (file "base.ss"))

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
  
  ;; generator-end : symbol
  (define generator-end (gensym 'generator-end))
  
  ; Syntax ---------------------------------------
  
  ;; syntax (gen-> contract)
  ;;
  ;; Expands into a contract that works with values and the generator-end
  ;; symbol.
  (define-syntax (gen-> stx)
    (syntax-case stx ()
      [(_ expr) #'(-> (or/c expr generator-end?))]))
  
  ; Core procedures ------------------------------
  
  ;; generator-end? : any -> boolean
  (define (generator-end? item)
    (eq? item generator-end))
  
  ;; generate-all : (list-of (gen-> any)) -> (list-of any)
  (define (generate-all gens)
    (map (lambda (item)
           (item))
         gens))
  
  ; Combinators ----------------------------------
  
  ;; generator-map : (a b c ... -> d) (gen-> a) (gen-> b) (gen-> c) ... -> (gen-> d)
  ;;
  ;; The generator equivalent of "map" from SRFI 1.
  ;;
  ;; Given a mapping function "fn" and some sources, creates a generator that returns:
  ;; 
  ;;     (apply fn sources)
  ;;
  ;; If, in a given iteration, any of the sources return generator-end, the mapping
  ;; function is not called, and the generator simply returns generator-end.
  (define (generator-map fn . gens)
    (let ([id (gensym)])
      (lambda ()
        (let ([args (generate-all gens)])
          (if (ormap generator-end? args)
              generator-end
              (apply fn args))))))
  
  ;; generator-fold : (a b c ... k -> k) k (gen-> a) (gen-> b) (gen-> c) ... -> (gen-> k)
  ;;
  ;; The generator equivalent of "fold" from SRFI 1.
  ;;
  ;; Given an iterator function "it", an initial accumulator and some sources,
  ;; creates a generator that returns:
  ;;
  ;;     (apply it (append sources (list accum)).
  ;;
  ;; The result is stored after each iteration and used as the accumulator for the
  ;; next iteration.
  ;;
  ;; If, in a given iteration, any of the sources return generator-end, the iterator
  ;; function is not called, and the generator simply returns generator-end.
  (define (generator-fold proc accum . gens)
    (lambda ()
      (let ([args (generate-all gens)])
        (if (ormap generator-end? args)
            generator-end
            (begin
              ; Update the accumulator...
              (set! accum (apply proc (append args (list accum))))
              ; ...and return it.
              accum)))))

  ;; generator-filter : (a -> boolean) (gen-> a) -> (gen-> a)
  ;;
  ;; The generator equivalent of "filter" from SRFI 1.
  ;;
  ;; Given a predicate "pred" and a source, creates a generator that returns
  ;; only those source values for which:
  ;;
  ;;     (pred source)
  ;;
  ;; is non-#f. Note that this means that a single call to the generator can result
  ;; in multiple calls to the source.
  ;;
  ;; If, in a given iteration, the source returns generator-end, the iterator
  ;; function is not called, and the generator simply returns generator-end.
  (define (generator-filter test gen)
    (letrec ([ans (lambda ()
                    (let ([arg (gen)])
                      (cond [(generator-end? arg) generator-end]
                            [(test arg)           arg]
                            [else                 (ans)])))])
      ans))
  
  ;; generator-filter-map : (a -> (U any #f)) (gen-> a) -> (gen-> any)
  ;;
  ;; The generator equivalent of "filter-map" from SRFI 1.
  ;;
  ;; Given a predicate "pred" and a source, creates a generator that returns non-#f
  ;; values of:
  ;;
  ;;     (pred source)
  ;;
  ;; Note that this means that a single call to the generator can result in 
  ;; multiple calls to the source.
  ;;
  ;; If, in a given iteration, the source returns generator-end, the iterator
  ;; function is not called, and the generator simply returns generator-end.
  (define (generator-filter-map test gen)
    (letrec ([ans (lambda ()
                    (let ([arg (gen)])
                      (if (generator-end? arg)
                          generator-end
                          (let ([answer (test arg)])
                            (if answer answer (ans))))))])
      ans))
  
  ;; generator-remove-duplicates : (gen-> a) -> (gen-> a)
  (define generator-remove-duplicates
    (let ([empty (gensym)])
      (opt-lambda (gen [same? equal?])
        (let ([last empty])
          (lambda ()
            (let loop ([curr (gen)])
              (cond [(generator-end? curr) generator-end]
                    [(same? last curr)     (set! last curr)
                                           (loop (gen))]
                    [else                  (set! last curr)
                                           curr])))))))
  
  ;; generator-debug : string (gen-> any) -> (gen-> any)
  ;;
  ;; Creates a generator that mimics its source, but prints generated values
  ;; as it goes.
  (define (generator-debug message generate)
    (lambda ()
      (let ([item (generate)])
        (printf "~a ~a~n" message item)
        item)))
  
  ; Accumulators and list interoperability -------
  
  ;; generator-for-each : (a b c ... -> void) (gen-> a) (gen-> b) (gen-> c) ... -> void
  ;;
  ;; Repeatedly calls source generators, supplying their values to an iterator
  ;; procedure, until one or more returns generator-end.
  (define (generator-for-each proc . gens)
    (let ([args (generate-all gens)])
      (if (ormap generator-end? args)
          (void)
          (begin (apply proc args)
                 (apply generator-for-each (cons proc gens))))))
  
  ;; generator-accumulate : (a b c ... k -> k) k (gen-> a) (gen-> b) (gen-> c) ... -> k
  ;;
  ;; Another equivalent of "fold" from SRFI 1.
  ;; 
  ;; Given an iterator function "it", an initial accumulator and some sources,
  ;; repeatedly does:
  ;;
  ;;     (apply it (append sources (list accum))
  ;;
  ;; until one or more of the sources returns generator-end. At this point the
  ;; accumulator is returned.
  (define (generator-accumulate proc accum0 . gens)
    (let loop ([accum accum0])
      (let ([args (generate-all gens)])
        (if (ormap generator-end? args)
            accum
            (loop (apply proc (append args (list accum))))))))
  
  ;; list->generator : (list-of a) -> (-> (U a generator-end))
  ;;
  ;; Creates a generator that iterates through the values in data and then
  ;; repeatedly returns end.
  (define (list->generator data)
    (lambda ()
      (if (null? data)
          generator-end
          (begin0 (car data)
                  (set! data (cdr data))))))
  
  ;; generator->list : (gen-> a) -> (list-of a)
  ;;
  ;; A convenient form of generator-accumulate that collects generated values
  ;; into a list.
  (define (generator->list gen)
    (reverse (generator-accumulate cons null gen)))
  
  ; Snooze specific (TODO : move to Snooze) ------
  
  ;; generator-project
  ;;     : (list-of boolean)
  ;;       (gen-> (list-of a))
  ;;       [(a a -> boolean)]
  ;;    -> (gen-> (append (list-of a) (list-of (list-of a))))
  ;;
  ;; Projects items from the supplied generator according to the rules
  ;; set out in project.ss.
  ;;
  ;; Passes non-list items straight through.
  (define generator-project
    (opt-lambda (mask generate [same? eq?])
      (define (projectable? x)
        (or (pair? x) (null? x)))
      (define collect-nonkeys
        (case-lambda 
          ((next-nonkeys nonkeys-accum)
           (if (andmap not next-nonkeys)
               nonkeys-accum
               (cons next-nonkeys nonkeys-accum)))
          ((next-nonkeys)
           (collect-nonkeys next-nonkeys null))))
      (yieldable yield
        (define (yield* last-keys last-nonkeys)
          (yield (append last-keys (list (reverse last-nonkeys)))))
        (lambda ()
          (let*-values ([(last)           (generate)]
                        [(keys0 nonkeys0) (if (list? last)
                                              (partition/mask last mask)
                                              (values #f null))])
            (let loop ([last last] [last-keys keys0] [last-nonkeys (collect-nonkeys nonkeys0)] [next (generate)])
              (let-values ([(next-keys next-nonkeys) 
                            (if (projectable? next)
                                (partition/mask next mask)
                                (values #f null))])
                (if (projectable? last)
                    (if (projectable? next)
                        (if (andmap same? last-keys next-keys)
                            (loop next next-keys (collect-nonkeys next-nonkeys last-nonkeys) (generate))
                            (begin (yield* last-keys last-nonkeys)
                                   (loop next next-keys (collect-nonkeys next-nonkeys) (generate))))
                        (begin (yield* last-keys last-nonkeys)
                               (loop next #f null (generate))))
                    (if (projectable? next)
                        (begin (yield last)
                               (loop next next-keys (collect-nonkeys next-nonkeys) (generate)))
                        (begin (yield last)
                               (loop next #f null (generate))))))))))))
  
  ; Provide statements ---------------------------
  
  ; "Normal" versions:
  
  (provide gen->
           generator-end 
           generator-end?)
  
  (provide/contract
   [generator-map               (->* (procedure?) (listof procedure?) (procedure?))]
   [generator-fold              (->* (procedure? any/c) (listof procedure?) (procedure?))]
   [generator-filter            (-> procedure? procedure? procedure?)]
   [generator-filter-map        (-> procedure? procedure? procedure?)]
   [generator-remove-duplicates (opt-> (procedure?) (procedure?) procedure?)]
   [generator-debug             (-> string? procedure? procedure?)]
   [generator-for-each          (->* (procedure?) (listof procedure?) any)]
   [generator-accumulate        (->* (procedure? any/c) (listof procedure?) any)]
   [generator->list             (-> procedure? (or/c pair? null?))]
   [list->generator             (-> (or/c pair? null?) procedure?)]
   [generator-project           (opt-> ((listof boolean?) procedure?) (procedure?) procedure?)])  
  )