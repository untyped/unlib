#lang scheme/base

(require "base.ss")

(require scheme/contract
         (rename-in srfi/19
                    [make-date      srfi-make-date]
                    [date->string   srfi-date->string]
                    [string->date   srfi-string->date]
                    [time-utc->date srfi-time-utc->date]
                    [time-tai->date srfi-time-tai->date])
         (rename-in (date-in)
                    [leap-year?     mzlib-leap-year?]
                    [date+          bzlib-date+])
         (date-tz-in)
         "debug.ss"
         (except-in "time.ss" copy-date))

; Token geographical bias:
(current-tz "GB")

; Constructors/converters ------------------------

; Creates a date representing the specified figures,
; interpreted relative to the immediate time zone offset in the relevant time zone.
;
; Involves some jiggery pokery because DST can cause the time zone offset to 
; differ between today and the specified date.
; 
; natural natural natural natural natural natural [#:tz string] -> date
(define (make-date nano second minute hour day month year #:tz [tz (current-tz)])
  ; 1. Work out the current time zone offset for tz.
  ; 2. Create a temporary date using that offset
  ;    this gets us to within one hour of the intended time.
  ; 3. Work out the correct offset at the intended time.
  ; 4. Rewrite the temporary date using the correct offset.
  (let* ([offset0 (date-zone-offset (current-date/tz tz))]
         [date0   (srfi-make-date nano second minute hour day month year offset0)]
         [offset1 (tz-offset date0 tz)])
    (srfi-make-date nano second minute hour day month year offset1)))

; Renders date to a string using the immediate time zone offset for the specified time zone.
;
; date string [#:tz string] -> string
(define (date->string date fmt #:tz [tz (current-tz)])
  (srfi-date->string (normalize-date date #:tz tz) fmt))

; Renders date to a string using the immediate time zone offset for the specified time zone.
;
; date string [#:tz string] -> string
(define (string->date str fmt #:tz [tz (current-tz)])
  ; If the string doesn't specify a time zone, 
  ; srfi-string->date creates a date using the current local TZ offset:
  (let ([temp (srfi-string->date str fmt)])
    ; We throw away the time zone offset that we get from SRFI 19 and use our own instead:
    (make-date (date-nanosecond temp)
               (date-second temp)
               (date-minute temp)
               (date-hour temp)
               (date-day temp)
               (date-month temp)
               (date-year temp)
               #:tz tz)))

; time-utc [#:tz string] -> date
(define (time-utc->date time #:tz [tz (current-tz)])
  (normalize-date (srfi-time-utc->date time 0) #:tz tz))

; time-tai [#:tz string] -> date
(define (time-tai->date time #:tz [tz (current-tz)])
  (normalize-date (srfi-time-tai->date time 0) #:tz tz))

;  date
;  [#:nanosecond natural]
;  [#:second     natural]
;  [#:minute     natural]
;  [#:hour       natural]
;  [#:day        natural]
;  [#:month      natural]
;  [#:year       integer]
;  [#:tz         string]
; ->
;  date
(define (copy-date date
                   #:nanosecond [nanosecond (date-nanosecond date)]
                   #:second     [second     (date-second     date)]
                   #:minute     [minute     (date-minute     date)]
                   #:hour       [hour       (date-hour       date)]
                   #:day        [day        (date-day        date)]
                   #:month      [month      (date-month      date)]
                   #:year       [year       (date-year       date)]
                   #:tz         [tz         (current-tz)])
  (make-date nanosecond second minute hour day month year #:tz tz))

; Date arithmetic --------------------------------

; date integer [#:tz string] -> date
(define (date+seconds date seconds #:tz [tz (current-tz)])
  (time-utc->date
   (add-duration (date->time-utc date)
                 (make-time time-duration 0 seconds))
   #:tz tz))

; date integer [#:tz string] -> date
(define (date+minutes date minutes #:tz [tz (current-tz)])
  (date+seconds date (* minutes 60) #:tz tz))

; date integer [#:tz string] -> date
(define (date+hours date hours #:tz [tz (current-tz)])
  (date+seconds date (* hours 60 60) #:tz tz))

; Adds/subtracts a number of days from the date, preserving the time-of-day in the immediate time zone.
;
; For example, 9am on Jan 1st (GMT) + 151 days = 9am on Jun 1st (BST).
;
; date integer [#:tz string] -> date
(define (date+days date days #:tz [tz (current-tz)])
  (date->tz (bzlib-date+ (normalize-date date #:tz tz) days) tz))

; Adds/subtracts a number of weeks from the date, preserving the time-of-day in the immediate time zone.
;
; date integer [#:tz string] -> date
(define (date+weeks date weeks #:tz [tz (current-tz)])
  (date+days date (* weeks 7) #:tz tz))

; Adds/subtracts a number of months from the date, preserving the day-of-the-month and 
; the time-of-day in the immediate time zone.
;
; For example, 9am on Jan 1st (GMT) + 6 months = 9am on Jun 1st (BST).
;
; date integer [#:tz string] -> date
(define (date+months date months #:tz [tz (current-tz)])
  (date+days date (months->days months (date-year date) (date-month date)) #:tz tz))

; Adds/subtracts a number of months from the date, preserving the day-of-the-month and 
; the time-of-day in the immediate time zone.
;
; For example, 9am on Jan 1st (GMT) + 6 months = 9am on Jun 1st (BST).
;
; date integer [#:tz string] -> date
(define (date+years date years #:tz [tz (current-tz)])
  (make-date (date-nanosecond date)
             (date-second     date)
             (date-minute     date)
             (date-hour       date)
             (date-day        date)
             (date-month      date)
             (+ (date-year date) years)
             #:tz tz))

; date [#:seconds integer] ... [#:tz string] -> date
(define (date+
         date
         #:seconds [seconds #f]
         #:minutes [minutes #f]
         #:hours   [hours   #f]
         #:days    [days    #f]
         #:weeks   [weeks   #f]
         #:months  [months  #f]
         #:years   [years   #f]
         #:tz      [tz      (current-tz)])
  (for/fold ([accum      date])
            ([combinator (in-list (list date+seconds date+minutes date+hours date+days date+weeks date+months date+years))]
             [amount     (in-list (list seconds minutes hours days weeks months years))])
            (if amount
                (combinator date amount #:tz tz)
                date)))

; Rewrites the supplied date using the immediate timezone offset for the specified time zone.
;
; For example, leaves 9am GMT on 1st December as it is, but rewrites 9am GMT on 1st June as 8am BST.
;
; date string -> date
(define (normalize-date date #:tz [tz (current-tz)])
  (let* ([offset (tz-offset date tz)])
    (if (= offset (date-zone-offset date))
        date
        (date->date/tz date offset))))

; Helpers ----------------------------------------

; Returns the number of days counting forward/backward *count* months from the first of *month* in *year*.
;
; integer natural natural [integer] -> integer
(define (months->days count year month [accum 0])
  (cond [(zero? count) accum]
        [(> count 0)   (if (= month 12)
                           (months->days (sub1 count) (add1 year) 1 (+ accum (days-in-month month year)))
                           (months->days (sub1 count) year (add1 month) (+ accum (days-in-month month year))))]
        [else          (if (= month 1)
                           (months->days (add1 count) (sub1 year) 12 (- accum (days-in-month 12 (sub1 year))))
                           (months->days (add1 count) year (sub1 month) (- accum (days-in-month (sub1 month) year))))]))

; Provides ---------------------------------------

(provide/contract
 [make-date      (->* (integer? integer? integer? integer? integer? integer? integer?) (#:tz zone-exists?) date?)]
 [copy-date      (->* (date?)
                      (#:nanosecond natural-number/c
                                    #:second natural-number/c
                                    #:minute natural-number/c
                                    #:hour   natural-number/c
                                    #:day    natural-number/c
                                    #:month  natural-number/c
                                    #:year   integer?
                                    #:tz     string?)
                      date?)]
 [date->string   (->* (date? string?) (#:tz zone-exists?) string?)]
 [string->date   (->* (string? string?) (#:tz zone-exists?) date?)]
 [time-utc->date (->* (time-utc?) (#:tz zone-exists?) date?)]
 [time-tai->date (->* (time-tai?) (#:tz zone-exists?) date?)]
 [date+seconds   (->* (date? integer?) (#:tz zone-exists?) date?)]
 [date+minutes   (->* (date? integer?) (#:tz zone-exists?) date?)]
 [date+hours     (->* (date? integer?) (#:tz zone-exists?) date?)]
 [date+days      (->* (date? integer?) (#:tz zone-exists?) date?)]
 [date+weeks     (->* (date? integer?) (#:tz zone-exists?) date?)]
 [date+months    (->* (date? integer?) (#:tz zone-exists?) date?)]
 [date+years     (->* (date? integer?) (#:tz zone-exists?) date?)]
 [date+          (->* (date?) (#:seconds natural-number/c
                                         #:minutes natural-number/c
                                         #:hours   natural-number/c
                                         #:days    natural-number/c
                                         #:weeks   natural-number/c
                                         #:months  natural-number/c
                                         #:years   natural-number/c
                                         #:tz      zone-exists?) date?)]
 [normalize-date (->* (date?) (#:tz zone-exists?) date?)])

(provide
 
 ; Constructors:
 make-time
 date->time-utc
 date->time-tai
 
 ; Time types:
 time-utc
 time-tai
 time-duration
 
 ; Predicates:
 date?
 date-week-day?
 time?
 time-utc?
 time-tai?
 time-duration?
 zone-exists?
 
 ; Accessors:
 current-time
 current-date
 time-type
 time-second
 time-nanosecond
 date-year
 date-month
 date-day
 date-hour
 date-minute
 date-second
 date-nanosecond
 date-zone-offset
 date-week-day
 date-week-day?
 date-day-of-the-week
 
 ; Comparisons:
 time<?
 time>?
 time<=?
 time>=?
 time=?
 
 ; Arithmetic:
 add-duration
 subtract-duration
 time-difference
 
 ; Conversions:
 date->time-utc
 date->time-tai
 
 ; Utilities:
 current-tz
 current-year
 current-time-zone-offset
 tz-names
 leap-year?
 days-in-year
 days-in-month)
