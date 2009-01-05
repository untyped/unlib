#lang scheme/base

(require srfi/26/cut
         (file "log.ss")
         (file "profile.ss")
         (file "test-base.ss"))

(provide profile-tests)

; Helpers ----------------------------------------

(define-timer outer-timer)
(define-timer inner-timer)

(define (outer)
  (sleep 1)
  (with-timer inner-timer
    (inner))
  (sleep 1))

(define (inner)
  (sleep 1))

; Tests ------------------------------------------

(define profile-tests
  (test-suite "profile.ss"
    
    '#:before (cut profile outer-timer outer)
    
    (test-case "outer-timer"
      (check > (timer-value outer-timer) 2000)
      (check < (timer-value outer-timer) 3000))
    
    (test-case "inner-timer"
      (check > (timer-value inner-timer) 1000)
      (check < (timer-value inner-timer) 2000))
    
    (test-case "timer custom-write"
      (check-pred (cut regexp-match #rx"#<timer:outer-timer 0m 2.[0-9][0-9][0-9]s>" <>) (format "~a" outer-timer))
      (check-pred (cut regexp-match #rx"#<timer:inner-timer 0m 1.[0-9][0-9][0-9]s>" <>) (format "~a" inner-timer)))
    
    ))

