#lang scheme/base

(require (only-in srfi/1/list lset=)
         srfi/26/cut
         (planet schematics/schemeunit/test)
         (file "cache-internal.ss"))

; Tests ------------------------------------------

(define cache-internal-tests
  (test-suite "cache-internal.ss"
    
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
    ))

; Provide statements -----------------------------

(provide cache-internal-tests)
