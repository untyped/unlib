#lang scheme/base

(require "base.ss"
         "lifebox.ss")

; type Key    : 'a
; type Value  : 'b
; type Load   : (Key -> Value)
; type Store  : (Key Value -> void)
; type Expire : (cache Key Value -> void)

; Structure types --------------------------------

; (struct Load Store Expire natural natural (hashof Key Value))
;
; Lifetime is the number of seconds, before values are
; expired and must be reloaded.
;
; Last-check is the last time, also in seconds, that expiry
; was checked.
(define-struct cache (load store expire lifetime last-check hash) #:transparent)

; Constructors -----------------------------------

; Load Store Expire natural -> cache
(define (create-cache load store expire lifetime)
  (create-cache/internal load store expire lifetime (make-hash)))

; Load Store Expire natural -> cache
(define (create-cacheeq load store expire lifetime)
  (create-cache/internal load store expire lifetime (make-hasheq)))

; Load Store Expire natural hash -> cache
(define (create-cache/internal load store expire lifetime hash)
  (define cache (make-cache load store expire lifetime (current-seconds) hash))
  (start-timer (/ (* lifetime 1000) 2)
               (cut cache-clean! cache))
  cache)

; Predicates -------------------------------------

; cache -> boolean
(define (cache-eq? cache)
  (hash-eq? (cache-hash cache)))

; Loading, storing and clearing ------------------

; cache key -> value
(define (cache-ref cache key)
  (define hash (cache-hash cache))
  (define load (cache-load cache))
  (define box  (hash-ref hash key #f))
  (define expired? (and box (lifebox-expired? box)))
  (cond [(not box)      (let* ([value (load key)]
                               [box   (create-box cache value)])
                          (hash-set! hash key box)
                          value)]
        [(not expired?) (lifebox-value box)] 
        [else           (cache-remove! cache key box)
                        ; This could loop forever if cache-expire inserts
                        ; an expired value into the cache, but that is
                        ; impossible given the API.
                        (cache-ref cache key)]))

; cache key value -> void
(define (cache-set! cache key value)
  (define hash  (cache-hash  cache))
  (define store (cache-store cache))
  (hash-set! hash key (create-box cache value))
  (store key value))

; cache -> void
(define (cache-clear! cache)
  (define hash (cache-hash cache))
  (hash-for-each hash (cut cache-remove! cache <> <>)))

; cache key lifebox -> void
(define (cache-remove! cache key box)
  (define hash (cache-hash cache))
  (begin (hash-remove! hash key)
         ((cache-expire cache) cache key (lifebox-value box))))
   
; cache -> void
(define (cache-clean! cache)
  (define hash (cache-hash cache))
  (hash-for-each
   hash
   (lambda (k v)
     (let ([now (current-seconds)])
       (when (lifebox-expired? v now)
         (cache-remove! cache k v))))))

; Utilities --------------------------------------

; cache value -> lifebox
(define (create-box cache value)
  (make-lifebox (+ (current-seconds) (cache-lifetime cache)) value))

; struct timer : channel
(define-struct timer (stop))

; natural (-> void) -> thread
;
; period is the time period between actions (in milliseconds).
(define (start-timer period action)
  (define stop (make-channel))
  (define (future-evt)
    (alarm-evt (+ (current-inexact-milliseconds) period)))
  (thread (lambda ()
            (let loop ([evt (sync (future-evt) stop)])
              (if (eq? evt 'stop)
                  (begin (void))
                  (begin (action)
                         (loop (sync (future-evt) stop)))))))
  (make-timer stop))

; timer -> void
(define (stop-timer timer)
  (channel-put (timer-stop timer) 'stop))

; Provide statements -----------------------------

(provide (rename-out [create-cache make-cache]
                     [create-cacheeq make-cacheeq])
         cache?
         cache-eq?
         cache-ref
         cache-set!
         cache-clear!
         cache-clean!
         start-timer
         stop-timer)
