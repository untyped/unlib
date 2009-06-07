#lang scheme/base

(require scheme/contract
         #;"base.ss"
         #;"number.ss"
         "profile-internal.ss")

; Split time -------------------------------------

; (thread-cell integer)
(define split-time-cell
  (make-thread-cell #f))

; -> (U integer #f)
(define (split-time-ref)
  (thread-cell-ref split-time-cell))

; integer -> void
(define (split-time-set! time)
  (thread-cell-set! split-time-cell time))

; Timers -----------------------------------------

; (listof timer)
(define *all-timers* null)

; symbol [number] -> timer
(define (create-timer name [value 0])
  (define ans (make-timer name value))
  (set! *all-timers* (append *all-timers* (list ans)))
  ans)

; (_ id)
(define-syntax define-timer
  (syntax-rules ()
    [(_ id)     (define id (create-timer 'id))]
    [(_ id val) (define id (create-timer 'id val))]))

; timer natural -> void
(define (timer-inc! timer increment)
  (set-timer-value! timer (+ (timer-value timer) increment)))

; timer -> void
(define (timer-reset! timer)
  (set-timer-value! timer 0))

; -> (listof timer)
(define (all-timers)
  *all-timers*)

; (parameter timer)
(define current-timer 
  (make-parameter (create-timer 'top)))

; Profiling --------------------------------------

; timer (arg ... -> ans) arg ... -> ans  
(define (profile timer fn . args)
  (dynamic-wind
   (lambda ()
     (define then (split-time-ref))
     (define now  (current-inexact-milliseconds))
     (when (and then (current-timer))
       (timer-inc! (current-timer) (- now then)))
     (split-time-set! now))
   (lambda ()
     (parameterize ([current-timer timer])
       (apply fn args)))
   (lambda ()
     (define then (split-time-ref))
     (define now  (current-inexact-milliseconds))
     (when then (timer-inc! timer (- now then)))
     (split-time-set! now))))

; (_ timer expr ...)
(define-syntax with-timer
  (syntax-rules ()
    [(_ timer expr ...)
     (profile timer (lambda () expr ...))]))

; Provide statements ---------------------------

(provide (except-out (struct-out timer) make-timer)
         with-timer
         define-timer)

(provide/contract
 [rename create-timer make-timer (-> symbol? timer?)]
 [timer-reset!                   (-> timer? void?)]
 [all-timers                     (-> (listof timer?))]
 [current-timer                  (-> timer?)]
 [profile                        (->* (timer? procedure?) () #:rest any/c any)])
