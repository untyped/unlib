#lang mzscheme

(require scheme/contract
         (file "base.ss")
         (file "list.ss"))

; pair ... -> hash-table
(define (make-hash-table/pairs . pairs)
  (let ([table (make-hash-table)])
    (alist-for-each 
     (lambda (key value)
       (hash-table-put! table key value))
     pairs)
    table))

; hash-table any -> boolean
(define (hash-table-mapped? table key)
  (with-handlers ([exn:unlib? (lambda (exn) #f)])
    (hash-table-get table key (lambda () (raise-exn exn:unlib "Not found.")))
    #t))

; hash-table -> (listof any)
(define (hash-table-keys table)
  (hash-table-map table (lambda (k v) k)))

; hash-table -> (listof any)
(define (hash-table-values table)
  (hash-table-map table (lambda (k v) v)))

(provide/contract
 [make-hash-table/pairs (->* () () #:rest (listof pair?) hash-table?)]
 [hash-table-mapped?    (-> hash-table? any/c boolean?)]
 [hash-table-keys       (-> hash-table? (or/c pair? null?))]
 [hash-table-values     (-> hash-table? (or/c pair? null?))])
