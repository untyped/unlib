(module project mzscheme
  
  (require (all-except (lib "contract.ss") any)
           (lib "etc.ss")
           (lib "pretty.ss")
           (lib "list.ss" "srfi" "1"))
  
  ;; item->keys : list (list-of boolean) -> list
  ;;
  ;; Data and mask must be the same length. Each member of mask corresponds to a member of data.
  ;;
  ;; Returns a list of all members of data for which the corresponding mask boolean is set.
  (define (filter-keys data mask)
    (filter-map (lambda (key? item)
                  (if key? item #f))
                mask
                data))
  
  ;; partition-data : list (list-of boolean) -> (values list list)
  ;;
  ;; Data and mask must be the same length. Each member of mask corresponds to a member of data.
  ;; 
  ;; Splits members of data into two lists: a list og members for which the corresponding mask bit is #t,
  ;; and a list of members for which the bit is #f.
  ;;
  ;; Returns both lists: the #t list first and the #f list second.
  (define (partition/mask data mask)
    (let loop ([data data] [mask mask] [accum1 null] [accum2 null])
      (if (null? data)
          (values (reverse accum1)
                  (reverse accum2))
          (if (car mask)
              (loop (cdr data)
                    (cdr mask)
                    (cons (car data) accum1)
                    accum2)
              (loop (cdr data)
                    (cdr mask)
                    accum1
                    (cons (car data) accum2))))))
  
  ;; project+fold
  ;;     : iterator
  ;;       accum
  ;;       (list-of (list-of (U key nonkey)))
  ;;       (list-of boolean)
  ;;       [ (key key -> boolean) ]
  ;;    -> accum
  ;;
  ;; where:
  ;;     iterator : (list key ... (list-of (list-of nonkey))) accum -> accum
  ;;     key      : any
  ;;     nonkey   : any
  ;;     accum    : any
  ;;
  ;; Iterates over the members of data:
  ;;   - Splits each member into a listof keys and a list of nonkeys (according to the mask):
  ;;         (list-of (U key nonkey)) -> (values (list-of key) (list-of nonkey))
  ;;   - For each sublist where adjacent members have matching keys, and accumulates their nonkeys into a list:
  ;;         -> (list key ... (list-of (list-of nonkey)))
  ;;   - Calls the iterator function, passing it this data structure and the current accumulator.
  ;;   - Continues iterating using the result of the iterator as the next accumulator.
  ;;   - When there is no more data, returns the final accumulator.
  (define project+fold
    (opt-lambda (proc initial-accum data mask [same? eq?])
      (if (null? data)
          null
          ; key-accum : (list-of any)
          ; rest-accum : (list-of (list-of any))
          ; ans-accum : (list-of (list any ... (list-of any)))
          ; 
          ; Stores the keys from the last data item processed. As long as the keys stay the same,
          ; we keep key-accum the same and add data to rest-accum. If any keys change, we add a
          ; new row to ans-accum, change keys-accum, and set rest-accum to #f.
          (let loop ([data data] [key-accum (filter-keys (car data) mask)] [nonkey-accum null] [ans-accum initial-accum])
             (if (null? data)
                 (proc (append key-accum (list (reverse nonkey-accum)))
                       ans-accum)
                 (let-values ([(keys nonkeys) (partition/mask (car data) mask)])
                   ;(printf "data    ~a~nmask    ~a~nkeys    ~a~nnonkeys ~a~nkeya    ~a~nnonkeya ~a~nansa    ~a~n" 
                   ;        data
                   ;        mask
                   ;        keys
                   ;        nonkeys
                   ;        key-accum
                   ;        nonkey-accum
                   ;        ans-accum)
                   (if (andmap same? keys key-accum)
                       (loop (cdr data)
                             key-accum
                             (cons nonkeys nonkey-accum)
                             ans-accum)
                       (loop (cdr data)
                             keys
                             (list nonkeys)
                             (proc (append key-accum (list (reverse nonkey-accum)))
                                   ans-accum)))))))))
  
  ;; project
  ;;     : (list-of (list-of (U key nonkey)))
  ;;       (list-of boolean)
  ;;       [ (key key -> boolean) ]
  ;;    -> accum
  ;;
  ;; Iterates over the members of data:
  ;;   - Splits each member into a list of keys and a list of nonkeys (according to the mask):
  ;;         (list-of (U key nonkey)) -> (values (list-of key) (list-of nonkey))
  ;;   - For each sublist where adjacent members have matching keys, and accumulates their nonkeys into a list:
  ;;         -> (list key ... (list-of (list-of nonkey)))
  ;;   - Accumulates and returns a list of these structures.
  (define project
    (opt-lambda (data mask [same? eq?])
      (reverse (project+fold cons null data mask same?))))
  
  ;; project+map
  ;;     : (key ... (list-of (list-of nonkey)) -> ans)
  ;;       (list-of (list-of (U key nonkey)))
  ;;       (list-of boolean)
  ;;       [ (key key -> boolean) ]
  ;;    -> (list-of ans)
  ;;
  ;; where:
  ;;     ans : any
  (define project+map
    (opt-lambda (proc data mask [same? eq?])
      (reverse (project+fold (lambda (data accum)
                               (cons (apply proc data) accum))
                             null
                             data
                             mask
                             same?))))
  
  ;; project+for-each
  ;;     : (key ... (list-of (list-of nonkey)) -> void)
  ;;       (list-of (list-of (U key nonkey)))
  ;;       (list-of boolean)
  ;;       [ (key key -> boolean) ]
  ;;    -> void
  ;;
  ;; where:
  ;;     ans : any
  (define project+for-each
    (opt-lambda (proc data mask [same? eq?])
      (reverse (project+fold (lambda (data accum)
                               (apply proc data))
                             (void)
                             data
                             mask
                             same?))
      (void)))
  
  ; Provide statements --------------------------- 
  
  (provide partition/mask)
  
  (provide/contract
   [project+fold (opt-> (procedure?
                         any/c
                         (or/c pair? null?)
                         (listof boolean?))
                        (procedure?)
                        any/c)]
   [project      (opt-> ((or/c pair? null?)
                         (listof boolean?))
                        (procedure?)
                        any/c)]
   [project+map  (opt-> (procedure?
                         (or/c pair? null?)
                         (listof boolean?))
                        (procedure?)
                        (or/c pair? null?))]
   [project+for-each (opt-> (procedure?
                             (or/c pair? null?)
                             (listof boolean?))
                            (procedure?)
                            any/c)])
  
  )
 