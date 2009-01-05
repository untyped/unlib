(module write-through-cache mzscheme

  (provide
   (rename create-write-through-cache make-write-through-cache)
   cache-get
   cache-set!
   cache-clear!)

  ;; struct write-through-cache : ('a -> 'b) ('a 'b -> void) (hash-of 'a 'b)
  (define-struct write-through-cache (load store hash))
  
  ;; create-write-through-cache : void -> cache
  (define (create-write-through-cache load store)
    (make-write-through-cache load store (make-hash-table 'weak)))

  ;; cache-get : cache key -> value
  (define (cache-get cache key)
    (let ([hash (write-through-cache-hash cache)]
          [load (write-through-cache-load cache)])
      (hash-table-get hash
                      key
                      (lambda ()
                        (let ([value (load key)])
                          (hash-table-put! hash key value)
                          value)))))

  ;; cache-set! : cache key value -> void
  (define (cache-set! cache key value)
    (let ([hash  (write-through-cache-hash  cache)]
          [store (write-through-cache-store cache)])
      (hash-table-put! hash key value)
      (store key value)))

  ;; cache-clear! : cache -> void
  (define (cache-clear! cache)
    (let ([hash (write-through-cache-hash cache)])
      (hash-table-for-each
       hash
       (lambda (k v) (hash-table-remove! hash k)))))
  )