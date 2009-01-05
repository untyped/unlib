#lang scheme/base

(require (except-in srfi/1 any)
         "base.ss")

; Procedures -------------------------------------

; list (listof boolean) -> list
;
; Data and mask must be the same length. Each member of mask corresponds to a member of data.
;
; Returns a list of all members of data for which the corresponding mask boolean is set.
(define (filter-keys data mask)
  (filter-map (lambda (key? item)
                (if key? item #f))
              mask
              data))

; list (listof boolean) -> (values list list)
;
; Data and mask must be the same length. Each member of mask corresponds to a member of data.
; 
; Splits members of data into two lists: a list og members for which the corresponding mask bit is #t,
; and a list of members for which the bit is #f.
;
; Returns both lists: the #t list first and the #f list second.
(define (partition/mask data mask)
  (let loop ([data data] [mask mask] [accum1 null] [accum2 null])
    (cond [(and (null? data) (null? mask))
           (values (reverse accum1)
                   (reverse accum2))]
          [(and (not (null? data)) (not (null? mask)))
           (if (car mask)
               (loop (cdr data)
                     (cdr mask)
                     (cons (car data) accum1)
                     accum2)
               (loop (cdr data)
                     (cdr mask)
                     accum1
                     (cons (car data) accum2)))]
          [else (raise-exn exn:fail:contract
                  (format "Expected data and mask of same length, received ~s and ~s" data mask))])))

;  iterator
;  accum
;  (listof (listof (U key nonkey)))
;  (listof boolean)
;  [ (key key -> boolean) ]
; ->
;  accum
;
; where iterator : (list key ... (listof (listof nonkey))) accum -> accum
;       key      : any
;       nonkey   : any
;       accum    : any
;
; Iterates over the members of data:
;   - Splits each member into a listof keys and a list of nonkeys (according to the mask):
;         (listof (U key nonkey)) -> (values (listof key) (listof nonkey))
;   - For each sublist where adjacent members have matching keys, and accumulates their nonkeys into a list:
;         -> (list key ... (listof (listof nonkey)))
;   - Calls the iterator function, passing it this data structure and the current accumulator.
;   - Continues iterating using the result of the iterator as the next accumulator.
;   - When there is no more data, returns the final accumulator.
(define (project+fold proc initial-accum data mask [same? eq?])
  (if (null? data)
      null
      ; key-accum  : (listof any)
      ; rest-accum : (listof (listof any))
      ; ans-accum  : (listof (list any ... (listof any)))
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
                              ans-accum))))))))

;  (listof (listof (U key nonkey)))
;  (listof boolean)
;  [ (key key -> boolean) ]
; ->
;  accum
;
; Iterates over the members of data:
;   - Splits each member into a list of keys and a list of nonkeys (according to the mask):
;         (listof (U key nonkey)) -> (values (listof key) (listof nonkey))
;   - For each sublist where adjacent members have matching keys, and accumulates their nonkeys into a list:
;         -> (list key ... (listof (listof nonkey)))
;   - Accumulates and returns a list of these structures.
(define (project data mask [same? eq?])
  (reverse (project+fold cons null data mask same?)))

;  (key ... (listof (listof nonkey)) -> ans)
;  (listof (listof (U key nonkey)))
;  (listof boolean)
;  [ (key key -> boolean) ]
; ->
;  (listof ans)
;
; where ans : any
(define (project+map proc data mask [same? eq?])
  (reverse (project+fold (lambda (data accum)
                           (cons (apply proc data) accum))
                         null
                         data
                         mask
                         same?)))

;  (key ... (listof (listof nonkey)) -> void)
;  (listof (listof (U key nonkey)))
;  (listof boolean)
;  [ (key key -> boolean) ]
; ->
;  void
;
; where ans : any
(define (project+for-each proc data mask [same? eq?])
  (project+fold (lambda (data accum)
                  (apply proc data))
                (void)
                data
                mask
                same?)
  (void))

; Provide statements --------------------------- 

(provide partition/mask)

(provide/contract
 [project+fold     (->* (procedure? any/c (or/c pair? null?) (listof boolean?))
                        (procedure?)
                        any)]
 [project          (->* ((or/c pair? null?) (listof boolean?))
                        (procedure?)
                        any)]
 [project+map      (->* (procedure? (or/c pair? null?) (listof boolean?))
                        (procedure?)
                        (or/c pair? null?))]
 [project+for-each (->* (procedure? (or/c pair? null?) (listof boolean?))
                        (procedure?)
                        any)])
