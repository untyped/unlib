#lang scheme/base

(require srfi/19
         "base.ss")

; Constants --------------------------------------

; integer
(define min-zone-offset
  (* 12 60 60 -1))

; integer
(define max-zone-offset 
  (* 12 60 60))

; Date and time constructors ---------------------

;  date
;  [#:nanosecond  (U integer #f)]
;  [#:second      (U integer #f)]
;  [#:minute      (U integer #f)]
;  [#:hour        (U integer #f)]
;  [#:day         (U integer #f)]
;  [#:month       (U integer #f)]
;  [#:year        (U integer #f)]
;  [#:zone-offset (U integer #f)]
; ->
;  srfi:date
(define (copy-date date
                   #:nanosecond  [nanosecond  #f] 
                   #:second      [second      #f]
                   #:minute      [minute      #f]
                   #:hour        [hour        #f]
                   #:day         [day         #f]
                   #:month       [month       #f]
                   #:year        [year        #f]
                   #:zone-offset [zone-offset #f])
  (make-date (or nanosecond  (date-nanosecond date))
             (or second      (date-second date))
             (or minute      (date-minute date))
             (or hour        (date-hour date))
             (or day         (date-day date))
             (or month       (date-month date))
             (or year        (date-year date))
             (or zone-offset (date-zone-offset date))))

; (U time-tai time-utc) -> date
(define (time->date time)
  (if (time-tai? time)
      (time-tai->date time)
      (time-utc->date time)))

; Date and time predicates -----------------------

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

; srfi:date -> boolean
(define (date-valid? date)
  (let ([nanosecond (date-nanosecond date)]
        [second     (date-second date)]
        [minute     (date-minute date)]
        [hour       (date-hour date)]
        [day        (date-day date)]
        [month      (date-month date)]
        [year       (date-year date)]
        [tz         (date-zone-offset date)])
    ; Check month first to prevent days-in-month 
    ; raising an exception if it is invalid:
    (and (>= month 1)            (<= month 12)
         (>= day 1)              (<= day (days-in-month month year))
         (>= hour 0)             (< hour   24)
         (>= minute 0)           (< minute 60)
         (>= second 0)           (< second 60)
         (>= nanosecond 0)       (< nanosecond 1000000000)
         (>= tz min-zone-offset) (< tz max-zone-offset))))

; Date and time accessors ------------------------

; date -> (U 'mon 'tue 'wed 'thu 'fri 'sat 'sun)
(define (date-day-of-the-week date)
  (case (date-week-day date)
    [(0) 'sun]
    [(1) 'mon]
    [(2) 'tue]
    [(3) 'wed]
    [(4) 'thu]
    [(5) 'fri]
    [(6) 'sat]))

; date -> boolean
(define (date-week-day? date)
  (let ([day (date-week-day date)])
    (and (>= day 1)
         (<= day 5))))

; Other utilities --------------------------------

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
    [else (raise-exn exn:fail:contract
            (format "Month out of range: ~a" month))]))

; integer [integer] [#:format string] -> string
;
; Takes an integer seconds value (like the value returned by current-seconds) and,
; optionally, a second argument representing the current seconds, and returns a string like:
;
;   n second(s) ago
;   n minute(s) ago
;   n hour(s) ago
;   n day(s) ago
(define (seconds->ago-string then [now (current-seconds)] #:format [format-string "~a ~a ago"] #:short? [short? #f])
  ; (integer string -> string)
  (define (make-answer number unit)
    (if (= number 1)
        (if (equal? unit "day")
            "yesterday"
            (format format-string number unit))
        (format format-string number (format "~as" unit))))
  ; integer
  (define difference (- now then))
  (when (< difference 0)
    (raise-exn exn:fail:contract
      (format "Expected first argument to be less than second, received ~a ~a." then now)))
  (cond [(< difference 60)    (make-answer difference (if short? "sec" "second"))]
        [(< difference 3600)  (make-answer (floor (/ difference 60)) (if short? "min" "minute"))]
        [(< difference 86400) (make-answer (floor (/ difference 3600)) (if short? "hr" "hour"))]
        [else                 (make-answer (floor (/ difference 86400)) "day")]))

; (U time-tai time-utc) [(U time-tai time-utc)] [#:format string] -> string
; 
; Takes a time-tai or time-utc (and, optionally, another argument of the same type representing
; the current time) and returns a string like:
;
;   n second(s) ago
;   n minute(s) ago
;   n hour(s) ago
;   n day(s) ago
(define (time->ago-string then [now (current-time (time-type then))] #:format [format-string "~a ~a ago"] #:short? [short? #f])
  (if (eq? (time-type then) (time-type now))
      (seconds->ago-string (time-second then) (time-second now) #:format format-string #:short? short?)
      (raise-exn exn:fail:contract
        (format "Arguments have different time types: ~a ~a" then now))))

; -> integer
; Returns the time zone offset of the current locale in seconds.
(define (current-time-zone-offset)
  (date-zone-offset (time-tai->date (current-time time-tai))))

; -> integer
(define (current-year)
  (date-year (time-tai->date (current-time time-tai))))

; time-utc string -> string
(define (time-utc->string time fmt)
  (date->string (time-utc->date time) fmt))
                                           
; time-tai string -> string
(define (time-tai->string time fmt)
  (date->string (time-tai->date time) fmt))

; Provide statements --------------------------- 

; contract
(define time/c
  (or/c time-tai? time-utc?))

; contract
(define month/c 
  (flat-named-contract 
   "month/c"
   (lambda (x)
     (and (integer? x)
          (>= x 1)
          (<= x 12)))))

; contract
(define day-of-the-week/c
  (flat-named-contract
   "day-of-the-week/c"
   (lambda (x)
     (and (memq x '(mon tue wed thu fri sat sun)) #t))))

(provide/contract
 [copy-date                (->* (date?)
                                (#:nanosecond (or/c (integer-in 0 999999999) #f)
                                              #:second      (or/c (integer-in 0 59) #f)
                                              #:minute      (or/c (integer-in 0 59) #f)
                                              #:hour        (or/c (integer-in 0 23) #f)
                                              #:day         (or/c (integer-in 1 31) #f)
                                              #:month       (or/c (integer-in 1 12) #f)
                                              #:year        (or/c integer? #f)
                                              #:zone-offset (or/c (integer-in min-zone-offset max-zone-offset) #f))
                                date?)]
 [time->date               (-> time/c date?)]
 [time-tai?                procedure?]
 [time-utc?                procedure?]
 [time-duration?           procedure?]
 [date-valid?              (-> date? boolean?)]
 [date-day-of-the-week     (-> date? day-of-the-week/c)]
 [date-week-day?           (-> date? boolean?)]
 [leap-year?               (-> integer? boolean?)]
 [days-in-month            (->* (month/c) (integer?) integer?)]
 [seconds->ago-string      (->* (integer?) (integer? #:format string? #:short? boolean?) string?)]
 [time->ago-string         (->* (time/c) (time/c #:format string? #:short? boolean?) string?)]
 [current-time-zone-offset (-> integer?)]
 [current-year             (-> integer?)]
 [time-utc->string         (-> time-utc? string? string?)]
 [time-tai->string         (-> time-tai? string? string?)])
