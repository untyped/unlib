#lang scheme/base

(require scheme/contract
         scheme/match
         srfi/19/time
         (file "base.ss"))

; any -> boolean
(define (time-tai? datum)
  (and (time? datum)
       (eq? (time-type datum) time-tai)))

; any -> boolean
(define (time-utc? datum)
  (and (time? datum)
       (eq? (time-type datum) time-utc)))

; any -> boolean
(define (time-duration? datum)
  (and (time? datum)
       (eq? (time-type datum) time-duration)))

; integer -> boolean
(define (leap-year? year)
  (if (zero? (remainder year 4))
      (if (zero? (remainder year 100))
          (if (zero? (remainder year 400))
              #t
              #f)
          #t)
      #f))

; integer [integer] -> integer
(define (days-in-month month [year 2001]) ; non-leap-year by default
  (case month
    [(9 4 6 11) 30]
    [(2) (if (leap-year? year) 29 28)]
    [(1 3 5 7 8 9 10 12) 31]
    [else (raise-exn exn:fail:unlib
            (format "Month out of range: ~a" month))]))

; srfi-date -> boolean
(define date-valid?
  (let ([min-tz (* 12 60 60 -1)]
        [max-tz (* 12 60 60)])
    (lambda (date)
      (let ([nanosecond (date-nanosecond date)]
            [second     (srfi:date-second date)]
            [minute     (srfi:date-minute date)]
            [hour       (srfi:date-hour date)]
            [day        (srfi:date-day date)]
            [month      (srfi:date-month date)]
            [year       (srfi:date-year date)]
            [tz         (date-zone-offset date)])
        ; Check month first to prevent days-in-month 
        ; raising an exception if it is invalid:
        (and (>= month 1)      (<= month 12)
             (>= day 1)        (<= day (days-in-month month year))
             (>= hour 0)       (< hour 24)
             (>= minute 0)     (< minute 60)
             (>= second 0)     (< second 60)
             (>= nanosecond 0) (< nanosecond 1000000000)
             (>= tz min-tz)    (< tz max-tz))))))

; time-tai -> boolean
(define (time-weekday? time)
  (define date
    (if (time-tai? time)
        (time-tai->date time)
        (time-utc->date time)))
  (if (member (date->string date "~a") '("Mon" "Tue" "Wed" "Thu" "Fri"))
      #t
      #f))

; (U time-tai time-utc) -> date
(define (time->date time)
  (if (time-tai? time)
      (time-tai->date time)
      (time-utc->date time)))

; integer [integer] -> string
;
; Takes an integer seconds value (like the value returned by current-seconds) and,
; optionally, a second argument representing the current seconds, and returns a string like:
;
;   n second(s) ago
;   n minute(s) ago
;   n hour(s) ago
;   n day(s) ago
(define (seconds->ago-string then [now (current-seconds)])
  ; (integer string -> string)
  (define (make-answer number unit)
    (if (= number 1)
        (if (equal? unit "day")
            "yesterday"
            (format "~a ~a ago" number unit))
        (format "~a ~as ago" number unit)))
  ; integer
  (define difference (- now then))
  (when (< difference 0)
    (raise-exn exn:fail:unlib
      (format "Expected first argument to be less than second, received ~a ~a." then now)))
  (cond [(< difference 60)    (make-answer difference "second")]
        [(< difference 3600)  (make-answer (floor (/ difference 60)) "minute")]
        [(< difference 86400) (make-answer (floor (/ difference 3600)) "hour")]
        [else                 (make-answer (floor (/ difference 86400)) "day")]))

; (U time-tai time-utc) [(U time-tai time-utc)] -> string
; 
; Takes a time-tai or time-utc (and, optionally, another argument of the same type representing
; the current time) and returns a string like:
;
;   n second(s) ago
;   n minute(s) ago
;   n hour(s) ago
;   n day(s) ago
(define time->ago-string
  (case-lambda
    [(then)
     (let ([now (if (time-tai? then)
                    (current-time time-tai)
                    (current-time time-utc))])
       (seconds->ago-string (time-second then) (time-second now)))]
    [(then now)
     (if (eq? (time-type then) (time-type now))
         (seconds->ago-string (time-second then) (time-second now))
         (raise-exn exn:fail:contract
           (format "Arguments have different time types: ~a ~a" then now)))]))

; -> integer
(define (current-time-zone-offset)
  (date-zone-offset (time-tai->date (current-time time-tai))))

; -> integer
(define (current-year)
  (srfi:date-year (time-tai->date (current-time time-tai))))

; Provide statements --------------------------- 

; (contract integer)
(define month/c 
  (and/c integer? (between/c 1 12)))

(provide time-tai?
         time-utc?
         time-duration?)

(provide/contract
 [leap-year?               (-> integer? boolean?)]
 [days-in-month            (->* (month/c) (integer?) integer?)]
 [date-valid?              (-> srfi:date? boolean?)]
 [time-weekday?            (-> (or/c time-tai? time-utc?) boolean?)]
 [time->date               (-> (or/c time-tai? time-utc?) srfi:date?)]
 [seconds->ago-string      (->* (integer?) (integer?) string?)]
 [time->ago-string         (->* ((or/c time-tai? time-utc?)) 
                                ((or/c time-tai? time-utc?))
                                string?)]
 [current-time-zone-offset (-> integer?)]
 [current-year             (-> integer?)])
