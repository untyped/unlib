(module write-through-cache-test mzscheme
  
  (require (file "test-base.ss")
           (file "write-through-cache.ss"))
  
  (provide write-through-cache-tests)

  (define loads null)
  (define saves null)
  (define counter 0)
  
  (define cache0 (make-write-through-cache
                  ;; Load
                  (lambda (key)
                    (set! loads (cons key loads))
                    (set! counter (add1 counter))
                    counter)
                  ;; Store
                  (lambda (key value)
                    (set! saves (cons (cons key value) saves)))))

  (define (clean-up)
    (set! loads null)
    (set! saves null)
    (set! counter 0)
    (cache-clear! cache0))

  (define-syntax cache-test-case
    (syntax-rules ()
      [(cache-test-case name expr0 expr1 ...)
       (test-case name (after expr0 expr1 ... (clean-up)))]))
  
  (define write-through-cache-tests
    (test-suite
     "All tests for write-through-cache"

     #:before clean-up
     #:after  clean-up

     (cache-test-case
      "Empty cache calls load"
      (check-equal? (cache-get cache0 'a) 1)
      (check-equal? loads '(a)))

     (cache-test-case
      "Populated cache doesn't call load"
      (cache-get cache0 'a)
      (check-equal? (cache-get cache0 'a) 1)
      (check-equal? loads '(a)))

     (cache-test-case
      "Store to cache writes through"
      (cache-set! cache0 'a 'foo)
      (check-equal? saves '((a . foo)))
      (check-equal? (cache-get cache0 'a) 'foo))
     
     ))
  )