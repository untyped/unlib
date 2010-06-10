#lang scheme/base

(require srfi/19
         "test-base.ss"
         "time.ss")

; Tests ------------------------------------------

(define/provide-test-suite time-tests
  
  (test-equal? "copy-date"
    (copy-date (make-date 1 2 3 4 5 6 7 8) 
               #:nanosecond  10
               #:second      20
               #:minute      30
               #:hour        11
               #:day         21
               #:month       12
               #:year        22
               #:zone-offset 32)
    (make-date 10 20 30 11 21 12 22 32))
  
  (test-case "time->date"
    (check-not-exn (cut time->date (current-time time-tai)))
    (check-not-exn (cut time->date (current-time time-utc)))
    (check-exn exn:fail:contract? (cut time->date (make-time time-duration 0 0))))
  
  (test-case "time-tai?"
    (check-equal? (map time-tai? (list (current-time time-tai)
                                       (current-time time-utc)
                                       (make-time time-duration 0 10)
                                       (current-seconds)
                                       #f))
                  (list #t #f #f #f #f)))
  
  (test-case "time-utc?"
    (check-equal? (map time-utc? (list (current-time time-tai)
                                       (current-time time-utc)
                                       (make-time time-duration 0 10)
                                       (current-seconds)
                                       #f))
                  (list #f #t #f #f #f)))
  
  (test-case "date-day-of-the-week"
    (check-eq? (date-day-of-the-week (make-date 0 0 0 12 26 05 2008 0)) 'mon)  ; a Monday
    (check-eq? (date-day-of-the-week (make-date 0 0 0 12 27 05 2008 0)) 'tue)  ; a Tuesday
    (check-eq? (date-day-of-the-week (make-date 0 0 0 12 28 05 2008 0)) 'wed)  ; a Wednesday
    (check-eq? (date-day-of-the-week (make-date 0 0 0 12 29 05 2008 0)) 'thu)  ; a Thursday
    (check-eq? (date-day-of-the-week (make-date 0 0 0 12 30 05 2008 0)) 'fri)  ; a Friday
    (check-eq? (date-day-of-the-week (make-date 0 0 0 12 31 05 2008 0)) 'sat)  ; a Saturday
    (check-eq? (date-day-of-the-week (make-date 0 0 0 12 01 06 2008 0)) 'sun)) ; a Sunday
  
  (test-case "date-week-day?"
    (check-true  (date-week-day? (make-date 0 0 0 12 26 05 2008 0)))  ; a Monday
    (check-true  (date-week-day? (make-date 0 0 0 12 27 05 2008 0)))  ; a Tuesday
    (check-true  (date-week-day? (make-date 0 0 0 12 28 05 2008 0)))  ; a Wednesday
    (check-true  (date-week-day? (make-date 0 0 0 12 29 05 2008 0)))  ; a Thursday
    (check-true  (date-week-day? (make-date 0 0 0 12 30 05 2008 0)))  ; a Friday
    (check-false (date-week-day? (make-date 0 0 0 12 31 05 2008 0)))  ; a Saturday
    (check-false (date-week-day? (make-date 0 0 0 12 01 06 2008 0)))) ; a Sunday
  
  (test-case "time-duration?"
    (check-equal? (map time-duration? (list (current-time time-tai)
                                            (current-time time-utc)
                                            (make-time time-duration 0 10)
                                            (current-seconds)
                                            #f))
                  (list #f #f #t #f #f)))
  
  (test-case "leap-year?"
    (check-equal? (leap-year? 2001) #f)
    (check-equal? (leap-year? 2004) #t)
    (check-equal? (leap-year? 2000) #t)
    (check-equal? (leap-year? 1900) #f))
  
  (test-case "days-in-year"
    (check-equal? (days-in-year 2001) 365)
    (check-equal? (days-in-year 2004) 366)
    (check-equal? (days-in-year 2000) 366)
    (check-equal? (days-in-year 1900) 365))
  
  (test-case "days-in-month"
    (check-equal? (days-in-month 1 2001) 31)
    (check-equal? (days-in-month 2 2001) 28)
    (check-equal? (days-in-month 3 2001) 31)
    (check-equal? (days-in-month 4 2001) 30)
    (check-equal? (days-in-month 5 2001) 31)
    (check-equal? (days-in-month 6 2001) 30)
    (check-equal? (days-in-month 7 2001) 31)
    (check-equal? (days-in-month 8 2001) 31)
    (check-equal? (days-in-month 9 2001) 30)
    (check-equal? (days-in-month 10 2001) 31)
    (check-equal? (days-in-month 11 2001) 30)
    (check-equal? (days-in-month 12 2001) 31)
    (check-equal? (days-in-month 1 2000) 31)
    (check-equal? (days-in-month 2 2000) 29)
    (check-equal? (days-in-month 3 2000) 31)
    (check-equal? (days-in-month 4 2000) 30)
    (check-equal? (days-in-month 5 2000) 31)
    (check-equal? (days-in-month 6 2000) 30)
    (check-equal? (days-in-month 7 2000) 31)
    (check-equal? (days-in-month 8 2000) 31)
    (check-equal? (days-in-month 9 2000) 30)
    (check-equal? (days-in-month 10 2000) 31)
    (check-equal? (days-in-month 11 2000) 30)
    (check-equal? (days-in-month 12 2000) 31))
  
  (test-case "date-valid?"
    (let ([check-date
           (lambda (nanosecond second minute hour day month year tz valid?)
             (check-equal? (date-valid? (make-date nanosecond second minute hour day month year tz)) valid?))])
      (check-date          0  0  0  0  1  1 2000      0 #t)
      (check-date         -1  0  0  0  1  1 2000      0 #f)
      (check-date          0 -1  0  0  1  1 2000      0 #f)
      (check-date          0  0 -1  0  1  1 2000      0 #f)
      (check-date          0  0  0 -1  1  1 2000      0 #f)
      (check-date          0  0  0  0  0  1 2000      0 #f)
      (check-date          0  0  0  0  1  0 2000      0 #f)
      (check-date          0  0  0  0  1  0 2000 -43201 #f)
      (check-date 1000000000  0  0  0  1  1 2000      0 #f)
      (check-date          0 60  0  0  1  1 2000      0 #f)
      (check-date          0  0 60  0  1  1 2000      0 #f)
      (check-date          0  0  0 24  1  1 2000      0 #f)
      (check-date          0  0  0  0 32  1 2000      0 #f)
      (check-date          0  0  0  0  1  0 2000      0 #f)
      (check-date          0  0  0  0  1  1 2000  43200 #f)))
  
  (test-case "seconds->ago-string"
    (let ([test-timestamp (current-seconds)])
      (check-equal? (seconds->ago-string test-timestamp test-timestamp) "0 seconds ago")
      (check-equal? (seconds->ago-string (- test-timestamp 1) test-timestamp) "1 second ago")
      (check-equal? (seconds->ago-string (- test-timestamp 59) test-timestamp) "59 seconds ago")
      (check-equal? (seconds->ago-string (- test-timestamp 60) test-timestamp) "1 minute ago")
      (check-equal? (seconds->ago-string (- test-timestamp 3599) test-timestamp) "59 minutes ago")
      (check-equal? (seconds->ago-string (- test-timestamp 3600) test-timestamp) "1 hour ago")
      (check-equal? (seconds->ago-string (- test-timestamp 86399) test-timestamp) "23 hours ago")
      (check-equal? (seconds->ago-string (- test-timestamp 86400) test-timestamp) "yesterday")
      (check-equal? (seconds->ago-string (- test-timestamp 172800) test-timestamp) "2 days ago")
      (check-equal? (seconds->ago-string (- test-timestamp 172800) test-timestamp #:format "~a ~a old") "2 days old")
      (check-equal? (seconds->ago-string (- test-timestamp 86400) test-timestamp #:format "~a ~a old") "yesterday")))
  
  (test-case "time->ago-string"
    (let ([test-timestamp (current-seconds)])
      (for ([time-type (in-list (list time-tai time-utc))])
        (with-check-info (['time-type time-type])
          (check-equal? (time->ago-string 
                         (make-time time-type 0 test-timestamp)
                         (make-time time-type 0 test-timestamp))
                        "0 seconds ago")
          (check-equal? (time->ago-string 
                         (make-time time-type 0 (- test-timestamp 60))
                         (make-time time-type 0 test-timestamp))
                        "1 minute ago")
          (check-equal? (time->ago-string 
                         (make-time time-type 0 (- test-timestamp 3600))
                         (make-time time-type 0 test-timestamp))
                        "1 hour ago")
          (check-equal? (time->ago-string 
                         (make-time time-type 0 (- test-timestamp 3600))
                         (make-time time-type 0 test-timestamp)
                         #:format "~a ~a old")
                        "1 hour old")))))
  
  (test-case "time->ago-string : mixture of time-tai and time-utc"
    (check-not-exn (cut time->ago-string (current-time time-tai)))
    (check-not-exn (cut time->ago-string (current-time time-utc)))
    (check-not-exn (cut time->ago-string (current-time time-tai) (current-time time-tai)))
    (check-not-exn (cut time->ago-string (current-time time-utc) (current-time time-utc)))
    (check-exn exn:fail:contract? (cut time->ago-string (current-time time-tai) (current-time time-utc)))
    (check-exn exn:fail:contract? (cut time->ago-string (current-time time-utc) (current-time time-tai)))))
