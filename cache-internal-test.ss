#lang scheme/base

(require (only-in srfi/1 lset=)
         "cache-internal.ss"
         "test-base.ss")

(define/provide-test-suite cache-internal-tests
  
  #:before (lambda () 
             (printf "Starting tests for cache-internal.ss (these will take a few seconds).~n"))
  
  #:after  (lambda ()
             (printf "Finished tests for cache-internal.ss.~n"))
  
  (test-case "timer-thread"
    (let* ([spaff null]
           [t (start-timer 200 (lambda () (set! spaff (cons 'foo spaff))))])
      (sleep 1)
      (stop-timer t)
      (check-false (null? spaff))
      (check-true (> (length spaff) 1))))
  
  (test-case "cache-clean!"
    (let* ([expired null]
           [c (make-cacheeq (lambda (k) k)
                            (lambda (k v) (void))
                            (lambda (c k v) (set! expired (cons k expired)))
                            0)])
      (cache-set! c 'a 'a)
      (cache-set! c 'b 'b)
      (cache-set! c 'c 'c)
      ; (sleep 1)
      (cache-clean! c)
      (check (cut lset= eq? <> <>) expired '(a b c))))
  
  (test-case "reaper thread removes items"
    (let* ([expired null]
           [c (make-cacheeq (lambda (k) k)
                            (lambda (k v) (void))
                            (lambda (c k v) (set! expired (cons k expired)))
                            0)])
      (cache-set! c 'a 'a)
      (cache-set! c 'b 'b)
      (cache-set! c 'c 'c)
      (sleep 1)
      (check (cut lset= eq? <> <>) expired '(b c a))))
  )