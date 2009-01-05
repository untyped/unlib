#lang scheme/base

(require (file "base.ss"))

; Constructors -----------------------------------

; alist -> hash
(define make-hash/alist
  (cut make-hash/internal make-hash <>))

; alist -> hash
(define make-hasheq/alist
  (cut make-hash/internal make-hasheq <>))

; alist -> hash
(define make-weak-hash/alist
  (cut make-hash/internal make-weak-hash <>))

; alist -> hash
(define make-weak-hasheq/alist
  (cut make-hash/internal make-weak-hasheq <>))

; (-> hash) alist -> hash
(define (make-hash/internal make-hash alist)
  (let ([hash (make-hash)])
    (for-each (lambda (item)
                (hash-set! hash (car item) (cdr item)))
              alist)
    hash))

; Accessors --------------------------------------

; hash any -> boolean
(define (hash-set? hash key)
  (and (hash-ref hash key #f) #t))

; hash -> (listof any)
(define (hash-keys hash)
  (hash-map hash (lambda (k v) k)))

; hash -> (listof any)
(define (hash-values hash)
  (hash-map hash (lambda (k v) v)))

; Provide statements -----------------------------

(provide/contract
 [make-hash/alist        (-> (listof pair?) (and/c hash? (not/c hash-eq?) (not/c hash-weak?)))]
 [make-hasheq/alist      (-> (listof pair?) (and/c hash? hash-eq?         (not/c hash-weak?)))]
 [make-weak-hash/alist   (-> (listof pair?) (and/c hash? (not/c hash-eq?) hash-weak?))]
 [make-weak-hasheq/alist (-> (listof pair?) (and/c hash? hash-eq?         hash-weak?))]
 [hash-set?              (-> hash? any/c boolean?)]
 [hash-keys              (-> hash? (or/c pair? null?))]
 [hash-values            (-> hash? (or/c pair? null?))])
