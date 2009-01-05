(module hash-table mzscheme
  
  (require (lib "etc.ss")
           (lib "string.ss" "srfi" "13")
           (file "base.ss")
           (file "list.ss"))
  
  (provide (all-defined))
  
  ;; make-hash-table/pairs : (cons any1 any2) ... -> hash-table
  (define (make-hash-table/pairs . pairs)
    (let ([table (make-hash-table)])
      (alist-for-each 
       (lambda (key value)
         (hash-table-put! table key value))
       pairs)
      table))
  
  ;; hash-table-mapped? : hash-table any -> boolean
  (define (hash-table-mapped? table key)
    (with-handlers ([exn:unlib? (lambda (exn) #f)])
      (hash-table-get table key (lambda () (raise-exn exn:unlib "Not found.")))
      #t))
  
  ;; hash-table-get/default table any1 any2 -> (U any any2)
  (define (hash-table-get/default table key default)
    (hash-table-get table key (lambda () default)))
  
  ;; hash-table-accessor : hash-table -> (id -> any)
  (define (hash-table-accessor table)
    (lambda (key)
      (hash-table-get 
       table 
       key 
       (lambda () 
         (raise-exn exn:fail:unlib
           (format "Key not found in hash-table: ~a" key))))))
  
  ;; hash-table-accessor/default : hash-table any -> (id -> any)
  (define (hash-table-accessor/default table default)
    (lambda (key)
      (hash-table-get/default 
       table 
       key 
       default)))
  
  ;; hash-table-put/append! : table any1 any2 -> nothing
  ;;
  ;; Looks up key in table. If it is present, checks it is mapped to a list of values.
  ;; If key is present, appends value to the mapped value list: if it isn't present,
  ;; puts a new value list containing only value.
  (define (hash-table-put/append! table key value)
    (let ([values (hash-table-get table key (lambda () null))])
      (if (list? values)
          (hash-table-put! table key (append values (list value)))
          (raise-exn exn:fail:unlib
            (format "Key not mapped to list: ~a ~a" key values)))))
  
  ;; hash-table-mutator : hash-table -> (any1 any2 -> nothing)
  (define (hash-table-mutator table)
    (lambda (key value)
      (hash-table-put! table key value)))
  
  ;; hash-table-mutator/append : hash-table -> (any1 any2 -> nothing)
  (define (hash-table-mutator/append table)
    (lambda (key value)
      (hash-table-put/append! table key value)))
  
  ;; hash-table-find : hash-table (any1 any2 -> (U any3 #f)) [(-> any4)] -> (U any3 any4)
  ;;
  ;; Applies the supplied procedure to every key/value pair in the hash-table,
  ;; and returns the first non-#f result. If no matching key/value pair is found,
  ;; the result of (default) is returned instead. The default value of (default) is,
  ;; of course, #f.
  (define hash-table-find
    (opt-lambda (table selector [default (lambda () #f)])
      (let ([ans #f])
        (with-handlers 
            ([exn:unlib? 
              (lambda (exn) 
                ans)])
          (hash-table-for-each
           table
           (lambda (key val)
             (let ([match (selector key val)])
               (if match
                   (set! ans match)
                   (raise-exn exn:unlib "")))))
          (default)))))
  
  ;; any-keys-have-values? : hash-table -> boolean
  (define (any-keys-have-values? table)
    (with-handlers ([exn:unlib? (lambda (exn) #t)])
      (hash-table-for-each
       table
       (lambda (key values)
         (if (list? values)
             (if (not (null? values))
                 (raise-exn exn:unlib 
                   "Breaking from for-each."))
             (raise-exn exn:fail:unlib 
               (format "Key not mapped to a list: ~a ~a" key values)))))
      #f))
  
  ;; key-has-values? : hash-table any -> boolean
  (define (key-has-values? table key)
    (let ([values (hash-table-get table key (lambda () null))])
      (if (list? values)
          (not (null? values))
          (raise-exn exn:fail:unlib 
            (format "Key not mapped to a list: ~a ~a" key values)))))
  
  (define hash-table->string 
    (opt-lambda (table [delimiter ", "])
      (string-join
       (hash-table-map
        table
        (lambda (key value)
          (format "~a=~a" key value)))
       delimiter)))
  
  )