#lang scheme/base

; (struct natural any)
;
; Seconds is the time in terms of current-seconds after
; which the value is considered expired.
(define-struct lifebox (seconds value))

; lifebox [natural] -> (U #t #f)
(define (lifebox-expired? lifebox [now (current-seconds)])
  (<= (lifebox-seconds lifebox) now))

; Provide statements -----------------------------

(provide make-lifebox
         lifebox?
         lifebox-expired?
         lifebox-value)
