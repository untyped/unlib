#lang scheme/base

(require "test-base.ss"
         "cache.ss")

; Helpers ----------------------------------------

(define loads null)
(define saves null)
(define counter 0)
(define expiries null)

(define cache0 
  (make-cacheeq
   ; Load
   (lambda (key)
     (set! loads (cons key loads))
     (set! counter (add1 counter))
     counter)
   ; Store
   (lambda (key value)
     (set! saves (cons (cons key value) saves)))))

(define cache1
  (make-cache
   ; Load
   (lambda (key)
     (set! loads (cons key loads))
     (set! counter (add1 counter))
     counter)
   ; Store
   (lambda (key value)
     (set! saves (cons (cons key value) saves)))
   #:expire  
   (lambda (c k v)
     (set! expiries (cons k expiries)))
   #:lifetime 1))

(define (clean-up)
  (cache-clear! cache0)
  (cache-clear! cache1)
  (set! loads null)
  (set! saves null)
  (set! counter 0)
  (set! expiries null))

(define-syntax cache-test-case
  (syntax-rules ()
    [(cache-test-case name expr0 expr1 ...)
     (test-case name (after expr0 expr1 ... (clean-up)))]))

; Tests ------------------------------------------

(define/provide-test-suite cache-tests
  
  #:before (lambda () 
             (clean-up)
             (printf "Starting tests for cache.ss (these will take a few seconds).~n"))
  
  #:after  (lambda ()
             (clean-up)
             (printf "Finished tests for cache.ss.~n"))
  
  (cache-test-case "Empty cache calls load"
    (check-equal? (cache-ref cache0 'a) 1)
    (check-equal? loads '(a)))
  
  (cache-test-case "Populated cache doesn't call load"
    (cache-ref cache0 'a)
    (check-equal? (cache-ref cache0 'a) 1)
    (check-equal? loads '(a)))
  
  (cache-test-case "Store to cache writes through"
    (cache-set! cache0 'a 'foo)
    (check-equal? saves '((a . foo)))
    (check-equal? (cache-ref cache0 'a) 'foo))
  
  (cache-test-case "Empty 'equal cache calls load"
    (set! loads null)
    (check-equal? (cache-ref cache1 "a") 1)
    (check-equal? loads '("a")))
  
  (cache-test-case "Populated 'equal cache doesn't call load"
    ; Store a then reset
    (cache-set! cache1 "a" 1)
    (set! loads '())
    (cache-ref cache1 "a")
    (check-equal? (cache-ref cache1 "a") 1)
    (check-equal? loads '()))
  
  (cache-test-case "Store to 'equal cache writes through"
    (cache-set! cache1 "a" 'foo)
    (check-equal? saves '(("a" . foo)))
    (check-equal? (cache-ref cache1 "a") 'foo))
  
  (cache-test-case "Expire function is called when lifetime exceeded"
    (cache-set! cache1 "a" 'foo)
    (sleep 2)
    (check-equal? (cache-ref cache1 "a") 1)
    (check-equal? expiries '("a")))
  
  )