#lang scheme/base

(require "test-base.ss")

(require (prefix-in srfi- srfi/19)
         "date.ss")

(require/expose "date.ss"
  (months->days))

; Helpers ----------------------------------------

; (U string #f) -> (U time-utc #f)
(define (st str)
  (and str (date->time-utc (string->date str "~Y-~m-~d ~H:~M"))))

; (U time-utc #f) -> (U string #f)
(define (ts time)
  (and time (date->string (time-utc->date time) "~Y-~m-~d ~H:~M")))

(define-syntax-rule (with-time-zones expr ...)
  (for ([tz (in-list (list "GB" "PST8PDT"))])
    (parameterize ([current-tz tz])
      (with-check-info (['current-tz tz])
        expr ...))))

(define-syntax-rule (with-time-zone tz expr ...)
  (parameterize ([current-tz tz])
    (with-check-info (['current-tz tz])
      expr ...)))

; Tests ------------------------------------------

(define/provide-test-suite date-tests
  
  ; Constructors ---------------------------------
  
  (test-case "make-date"
    ; Day before and day of a TZ change in GB:
    ; GB changes from GMT to BST and from BST to GMT at 1am GMT.
    ;
    ; We check an hour before and an hour after each change to make sure the correct time zones are returned.
    ; If we were to check the hour of the change, our current time zone is selected so the results of the test vary
    ; depending on when we run it.
    (parameterize ([current-tz "GB"])
      (check-equal? (make-date 0 00 00 00 28 03 2010) (srfi-make-date 0 00 00 00 28 03 2010    0))
      (check-equal? (make-date 0 00 00 02 28 03 2010) (srfi-make-date 0 00 00 02 28 03 2010 3600))
      (check-equal? (make-date 0 00 00 00 31 10 2010) (srfi-make-date 0 00 00 00 31 10 2010 3600))
      (check-equal? (make-date 0 00 00 02 31 10 2010) (srfi-make-date 0 00 00 02 31 10 2010    0))
      (check-equal? (make-date 0 00 00 00 27 03 2011) (srfi-make-date 0 00 00 00 27 03 2011    0))
      (check-equal? (make-date 0 00 00 02 27 03 2011) (srfi-make-date 0 00 00 02 27 03 2011 3600))
      (check-equal? (make-date 0 00 00 00 30 10 2011) (srfi-make-date 0 00 00 00 30 10 2011 3600))
      (check-equal? (make-date 0 00 00 02 30 10 2011) (srfi-make-date 0 00 00 02 30 10 2011    0)))
    ; Hour before and hour of a TZ change in PST8PDT.
    ; PST8PDT changes from normal to DST at 2am normal, and DST to normal at 2am DST.
    ;
    ; We check an hour before and an hour after each change to make sure the correct time zones are returned.
    ; If we were to check the hour of the change, our current time zone is selected so the results of the test vary
    ; depending on when we run it.
    (parameterize ([current-tz "PST8PDT"])
      (check-equal? (make-date 0 00 00 01 14 03 2010) (srfi-make-date 0 00 00 01 14 03 2010 -28800))
      (check-equal? (make-date 0 00 00 03 14 03 2010) (srfi-make-date 0 00 00 03 14 03 2010 -25200))
      (check-equal? (make-date 0 00 00 00 07 11 2010) (srfi-make-date 0 00 00 00 07 11 2010 -25200))
      (check-equal? (make-date 0 00 00 02 07 11 2010) (srfi-make-date 0 00 00 02 07 11 2010 -28800))
      (check-equal? (make-date 0 00 00 01 13 03 2011) (srfi-make-date 0 00 00 01 13 03 2011 -28800))
      (check-equal? (make-date 0 00 00 03 13 03 2011) (srfi-make-date 0 00 00 03 13 03 2011 -25200))
      (check-equal? (make-date 0 00 00 00 06 11 2011) (srfi-make-date 0 00 00 00 06 11 2011 -25200))
      (check-equal? (make-date 0 00 00 02 06 11 2011) (srfi-make-date 0 00 00 02 06 11 2011 -28800))))
  
  (test-case "copy-date"
    (parameterize ([current-tz "GB"])
      (let ([date (make-date 0 00 00 00 01 01 2000)])
        (check-equal? (copy-date date
                                 #:nanosecond 1
                                 #:second     2
                                 #:minute     3
                                 #:hour       4
                                 #:day        5
                                 #:month      3
                                 #:year       2007)
                      (srfi-make-date 1 02 03 04 05 03 2007 0))
        (check-equal? (copy-date date
                                 #:nanosecond 1
                                 #:second     2
                                 #:minute     3
                                 #:hour       4
                                 #:day        5
                                 #:month      6
                                 #:year       2007)
                      (srfi-make-date 1 02 03 04 05 06 2007 3600))))
    (parameterize ([current-tz "PST8PDT"])
      (let ([date (make-date 0 00 00 00 01 01 2000)])
        (check-equal? (copy-date date
                                 #:nanosecond 1
                                 #:second     2
                                 #:minute     3
                                 #:hour       4
                                 #:day        5
                                 #:month      3
                                 #:year       2007)
                      (srfi-make-date 1 02 03 04 05 03 2007 -28800))
        (check-equal? (copy-date date
                                 #:nanosecond 1
                                 #:second     2
                                 #:minute     3
                                 #:hour       4
                                 #:day        5
                                 #:month      6
                                 #:year       2007)
                      (srfi-make-date 1 02 03 04 05 06 2007 -25200)))))
  
  (test-case "date->string"
    ; Dates specified with the local time zone offset:
    ;
    ; We check an hour before and an hour after each change to make sure the correct time zones are returned.
    ; If we were to check the hour of the change, our current time zone is selected so the results of the test vary
    ; depending on when we run it.
    (check-equal? (date->string (make-date 0 00 00 00 28 03 2010) "~Y-~m-~d ~H:~M") "2010-03-28 00:00")
    ;(check-equal? (date->string (make-date 0 00 00 01 28 03 2010) "~Y-~m-~d ~H:~M") "2010-03-28 02:00") ; this is the hour of the change
    (check-equal? (date->string (make-date 0 00 00 02 28 03 2010) "~Y-~m-~d ~H:~M") "2010-03-28 02:00")
    ; Dates specified with other offsets:
    (check-equal? (date->string (srfi-make-date 0 00 00 09 01 01 2010 3600) "~Y-~m-~d ~H:~M") "2010-01-01 08:00")
    (check-equal? (date->string (srfi-make-date 0 00 00 09 01 07 2010    0) "~Y-~m-~d ~H:~M") "2010-07-01 10:00"))
  
  (test-case "string->date"
    ; We check an hour before and an hour after each change to make sure the correct time zones are returned.
    ; If we were to check the hour of the change, our current time zone is selected so the results of the test vary
    ; depending on when we run it.
    (parameterize ([current-tz "GB"])
      (check-equal? (string->date "2010-03-28 00:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 00 28 03 2010    0))
      (check-equal? (string->date "2010-03-28 02:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 02 28 03 2010 3600))
      (check-equal? (string->date "2010-10-31 00:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 00 31 10 2010 3600))
      (check-equal? (string->date "2010-10-31 02:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 02 31 10 2010    0))
      (check-equal? (string->date "2011-03-27 00:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 00 27 03 2011    0))
      (check-equal? (string->date "2011-03-27 02:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 02 27 03 2011 3600))
      (check-equal? (string->date "2011-10-30 00:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 00 30 10 2011 3600))
      (check-equal? (string->date "2011-10-30 02:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 02 30 10 2011    0)))
    ; We check an hour before and an hour after each change to make sure the correct time zones are returned.
    ; If we were to check the hour of the change, our current time zone is selected so the results of the test vary
    ; depending on when we run it.
    (parameterize ([current-tz "PST8PDT"])
      (check-equal? (string->date "2010-03-14 01:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 01 14 03 2010 -28800))
      (check-equal? (string->date "2010-03-14 03:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 03 14 03 2010 -25200))
      (check-equal? (string->date "2010-11-07 00:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 00 07 11 2010 -25200))
      (check-equal? (string->date "2010-11-07 02:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 02 07 11 2010 -28800))
      (check-equal? (string->date "2011-03-13 01:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 01 13 03 2011 -28800))
      (check-equal? (string->date "2011-03-13 03:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 03 13 03 2011 -25200))
      (check-equal? (string->date "2011-11-06 00:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 00 06 11 2011 -25200))
      (check-equal? (string->date "2011-11-06 02:00" "~Y-~m-~d ~H:~M") (srfi-make-date 0 00 00 02 06 11 2011 -28800))))
  
  (test-case "string->date : omitting day/month/year (see comments for details)"
    (parameterize ([current-tz "PST8PDT"])
      ; Two rules:
      ;   - missing date components should be 0, not #t as returned by SRFI 19's string->date;
      ;   - if any date components are missing, the time zone should be 0.
      (check-equal? (string->date "01:02"            "~H:~M")          (srfi-make-date 0 00 02 01 00 00 0000 -28800))
      (check-equal? (string->date "01:02 03"         "~H:~M ~d")       (srfi-make-date 0 00 02 01 03 00 0000 -28800))
      (check-equal? (string->date "01:02 03/04"      "~H:~M ~d/~m")    (srfi-make-date 0 00 02 01 03 04 0000 -28800))
      (check-equal? (string->date "01:02 03/04/2005" "~H:~M ~d/~m/~Y") (srfi-make-date 0 00 02 01 03 04 2005 -28800))))
  
  (test-case "time-utc->date, time-tai->date"
    (for ([convert   (in-list (list (compose time-utc->date date->time-utc)
                                    (compose time-tai->date date->time-tai)))]
          [time-type (in-list (list time-utc time-tai))])
      (with-check-info (['time-type time-type])
        (parameterize ([current-tz "GB"])
          ; Original date specified with current time zone offset:
          (check-equal? (convert (srfi-make-date 0 00 00 00 28 03 2010    0)) (srfi-make-date 0 00 00 00 28 03 2010    0))
          (check-equal? (convert (srfi-make-date 0 00 00 01 28 03 2010    0)) (srfi-make-date 0 00 00 02 28 03 2010 3600))
          (check-equal? (convert (srfi-make-date 0 00 00 02 28 03 2010    0)) (srfi-make-date 0 00 00 03 28 03 2010 3600))
          (check-equal? (convert (srfi-make-date 0 00 00 00 28 03 2010 3600)) (srfi-make-date 0 00 00 23 27 03 2010    0))
          (check-equal? (convert (srfi-make-date 0 00 00 01 28 03 2010 3600)) (srfi-make-date 0 00 00 00 28 03 2010    0))
          (check-equal? (convert (srfi-make-date 0 00 00 02 28 03 2010 3600)) (srfi-make-date 0 00 00 02 28 03 2010 3600))
          ; Original date specified with different time zone offset:
          (check-equal? (convert (srfi-make-date 0 00 00 01 28 03 2010 3600)) (srfi-make-date 0 00 00 00 28 03 2010    0))
          (check-equal? (convert (srfi-make-date 0 00 00 02 28 03 2010    0)) (srfi-make-date 0 00 00 03 28 03 2010 3600)))
        (parameterize ([current-tz "PST8PDT"])
          ; Original date specified with current time zone offset:
          (check-equal? (convert (srfi-make-date 0 00 00 01 14 03 2010 -28800)) (srfi-make-date 0 00 00 01 14 03 2010 -28800))
          (check-equal? (convert (srfi-make-date 0 00 00 02 14 03 2010 -28800)) (srfi-make-date 0 00 00 03 14 03 2010 -25200))
          (check-equal? (convert (srfi-make-date 0 00 00 03 14 03 2010 -28800)) (srfi-make-date 0 00 00 04 14 03 2010 -25200))
          (check-equal? (convert (srfi-make-date 0 00 00 01 14 03 2010 -25200)) (srfi-make-date 0 00 00 00 14 03 2010 -28800))
          (check-equal? (convert (srfi-make-date 0 00 00 02 14 03 2010 -25200)) (srfi-make-date 0 00 00 01 14 03 2010 -28800))
          (check-equal? (convert (srfi-make-date 0 00 00 03 14 03 2010 -25200)) (srfi-make-date 0 00 00 03 14 03 2010 -25200))
          ; Original date specified with different time zone offset:
          (check-equal? (convert (srfi-make-date 0 00 00 02 14 03 2010 -25200)) (srfi-make-date 0 00 00 01 14 03 2010 -28800))
          (check-equal? (convert (srfi-make-date 0 00 00 03 14 03 2010 -28800)) (srfi-make-date 0 00 00 04 14 03 2010 -25200))))))
  
  ; Arithmetic -----------------------------------
  
  (test-case "months->days"
    ; Zero:
    (check-equal? (months->days   0 2010  1)   0)
    ; Positive:
    (check-equal? (months->days   1 2010  1)  31)
    (check-equal? (months->days   2 2010  1)  59)
    (check-equal? (months->days  12 2010  1) 365)
    (check-equal? (months->days   3 2010 11)  92)
    ; Leap year:
    (check-equal? (months->days   1 2012  1)  31)
    (check-equal? (months->days   2 2012  1)  60)
    (check-equal? (months->days  12 2012  1) 366)
    (check-equal? (months->days  12 2012  3) 365)
    ; Negative:
    (check-equal? (months->days  -1 2010  1)  -31)
    (check-equal? (months->days  -2 2010  1)  -61)
    (check-equal? (months->days -12 2010  1) -365)
    (check-equal? (months->days  -3 2010 11)  -92)
    ; Negative leap year:
    (check-equal? (months->days  -2 2012  2)  -62)
    (check-equal? (months->days  -2 2012  3)  -60)
    (check-equal? (months->days -12 2012  3) -366)
    (check-equal? (months->days -12 2012  2) -365))
  
  (test-case "date+seconds"
    (check-equal? (date+seconds (srfi-make-date 0 00 00 09 01 01 2010 0)     1) (srfi-make-date 0 01 00 09 01 01 2010    0))
    (check-equal? (date+seconds (srfi-make-date 0 00 00 09 01 01 2010 0)    61) (srfi-make-date 0 01 01 09 01 01 2010    0))
    (check-equal? (date+seconds (srfi-make-date 0 00 00 09 01 01 2010 0)  3601) (srfi-make-date 0 01 00 10 01 01 2010    0))
    (check-equal? (date+seconds (srfi-make-date 0 00 00 09 01 01 2010 0) 86401) (srfi-make-date 0 01 00 09 02 01 2010    0))
    ; Time zone change:
    (check-equal? (date+seconds (srfi-make-date 0 00 00 09 27 03 2010 0) 86400) (srfi-make-date 0 00 00 10 28 03 2010 3600)))
  
  (test-case "date+minutes"
    (check-equal? (date+minutes (srfi-make-date 0 00 00 09 01 01 2010 0)    1) (srfi-make-date 0 00 01 09 01 01 2010    0))
    (check-equal? (date+minutes (srfi-make-date 0 00 00 09 01 01 2010 0)   61) (srfi-make-date 0 00 01 10 01 01 2010    0))
    (check-equal? (date+minutes (srfi-make-date 0 00 00 09 01 01 2010 0) 1441) (srfi-make-date 0 00 01 09 02 01 2010    0))
    ; Time zone change:
    (check-equal? (date+minutes (srfi-make-date 0 00 00 09 27 03 2010 0) 1440) (srfi-make-date 0 00 00 10 28 03 2010 3600)))
  
  (test-case "date+hours"
    (check-equal? (date+hours (srfi-make-date 0 00 00 09 01 01 2010 0)  1) (srfi-make-date 0 00 00 10 01 01 2010    0))
    (check-equal? (date+hours (srfi-make-date 0 00 00 09 01 01 2010 0) 25) (srfi-make-date 0 00 00 10 02 01 2010    0))
    ; Time zone change:
    (check-equal? (date+hours (srfi-make-date 0 00 00 09 27 03 2010 0) 24) (srfi-make-date 0 00 00 10 28 03 2010 3600)))
  
  (test-case "date+days"
    (check-equal? (date+days (srfi-make-date 0 00 00 09 01 01 2010    0)  1) (srfi-make-date 0 00 00 09 02 01 2010    0))
    (check-equal? (date+days (srfi-make-date 0 00 00 09 01 01 2010    0) 31) (srfi-make-date 0 00 00 09 01 02 2010    0))
    (check-equal? (date+days (srfi-make-date 0 00 00 09 26 03 2010    0)  1) (srfi-make-date 0 00 00 09 27 03 2010    0))
    ; Time zone change:
    (check-equal? (date+days (srfi-make-date 0 00 00 09 27 03 2010    0)  1) (srfi-make-date 0 00 00 09 28 03 2010 3600))
    ; Input times are adjusted to the local timezone before doing any arithmetic:
    (check-equal? (date+days (srfi-make-date 0 00 00 09 27 03 2010 3600)  1) (srfi-make-date 0 00 00 08 28 03 2010 3600)))
  
  (test-case "date+weeks"
    (check-equal? (date+weeks (srfi-make-date 0 00 00 09 01 01 2010    0)  1) (srfi-make-date 0 00 00 09 08 01 2010    0))
    ; Time zone change:
    (check-equal? (date+weeks (srfi-make-date 0 00 00 09 22 03 2010    0)  1) (srfi-make-date 0 00 00 09 29 03 2010 3600))
    ; Input times are adjusted to the local timezone before doing any arithmetic:
    (check-equal? (date+weeks (srfi-make-date 0 00 00 09 22 03 2010 3600)  1) (srfi-make-date 0 00 00 08 29 03 2010 3600)))
  
  (test-case "date+months"
    (check-equal? (date+months (srfi-make-date 0 00 00 09 01 01 2010    0) 1) (srfi-make-date 0 00 00 09 01 02 2010    0))
    ; Time zone change:
    (check-equal? (date+months (srfi-make-date 0 00 00 09 01 01 2010    0) 6) (srfi-make-date 0 00 00 09 01 07 2010 3600))
    ; Input times are adjusted to the local timezone before doing any arithmetic:
    (check-equal? (date+months (srfi-make-date 0 00 00 09 01 01 2010 3600) 6) (srfi-make-date 0 00 00 08 01 07 2010 3600))
    ; Leap years are taken into account:
    (check-equal? (date+months (srfi-make-date 0 00 00 09 01 01 2012    0) 6) (srfi-make-date 0 00 00 09 01 07 2012 3600)) ; divisible by 4
    (check-equal? (date+months (srfi-make-date 0 00 00 09 01 01 2100    0) 6) (srfi-make-date 0 00 00 09 01 07 2100 3600)) ; divisible by 100
    (check-equal? (date+months (srfi-make-date 0 00 00 09 01 01 2000    0) 6) (srfi-make-date 0 00 00 09 01 07 2000 3600)) ; divisible by 400
    ; Days above 28 are handled correctly:
    (check-equal? (date+months (srfi-make-date 0 00 00 09 29 01 2010    0) 1) (srfi-make-date 0 00 00 09 01 03 2010    0))
    (check-equal? (date+months (srfi-make-date 0 00 00 09 29 01 2010    0) 2) (srfi-make-date 0 00 00 09 29 03 2010 3600))
    (check-equal? (date+months (srfi-make-date 0 00 00 09 29 01 2012    0) 1) (srfi-make-date 0 00 00 09 29 02 2012    0))
    (check-equal? (date+months (srfi-make-date 0 00 00 09 29 01 2012    0) 2) (srfi-make-date 0 00 00 09 29 03 2012 3600)))
  
  (test-case "date+years"
    (check-equal? (date+years (srfi-make-date 0 00 00 09 01 01 2010    0)     1) (srfi-make-date 0 00 00 09 01 01 2011    0))
    (check-equal? (date+years (srfi-make-date 0 00 00 09 01 01 2010    0)   -11) (srfi-make-date 0 00 00 09 01 01 1999    0))
    ; Too early for official date records - the -75 is "Local Mean Time" for London:
    (check-equal? (date+years (srfi-make-date 0 00 00 09 01 01 2010 3600) -2011) (srfi-make-date 0 00 00 09 01 01   -1  -75)))
  
  (test-case "normalize-date"
    (with-time-zone "GB"
      (check-equal? (normalize-date (srfi-make-date 0 00 00 09 01 01 2010 3600)) (srfi-make-date 0 00 00 08 01 01 2010    0))
      (check-equal? (normalize-date (srfi-make-date 0 00 00 09 01 07 2010    0)) (srfi-make-date 0 00 00 10 01 07 2010 3600)))
    (with-time-zone "PST8PDT"
      (check-equal? (normalize-date (srfi-make-date 0 00 00 09 01 01 2010 3600)) (srfi-make-date 0 00 00 00 01 01 2010 -28800))
      (check-equal? (normalize-date (srfi-make-date 0 00 00 09 01 07 2010    0)) (srfi-make-date 0 00 00 02 01 07 2010 -25200)))))
