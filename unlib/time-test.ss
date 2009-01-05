#lang mzscheme

(require srfi/19/time
         srfi/26/cut)

(require (file "test-base.ss")
         (file "time.ss"))

(provide time-tests)

(define test-timestamp (current-seconds))

(define time-tests
  (test-suite "time.ss"
    
    (test-case "leap-year?"
      (check-equal? (leap-year? 2001) #f "2001")
      (check-equal? (leap-year? 2004) #t "2004")
      (check-equal? (leap-year? 2000) #t "2000")
      (check-equal? (leap-year? 1900) #f "1900"))
    
    (test-case "days-in-month"
      (check-equal? (days-in-month 1 2001) 31 "Jan non-leap")
      (check-equal? (days-in-month 2 2001) 28 "Feb non-leap")
      (check-equal? (days-in-month 3 2001) 31 "Mar non-leap")
      (check-equal? (days-in-month 4 2001) 30 "Apr non-leap")
      (check-equal? (days-in-month 5 2001) 31 "May non-leap")
      (check-equal? (days-in-month 6 2001) 30 "Jun non-leap")
      (check-equal? (days-in-month 7 2001) 31 "Jul non-leap")
      (check-equal? (days-in-month 8 2001) 31 "Aug non-leap")
      (check-equal? (days-in-month 9 2001) 30 "Sep non-leap")
      (check-equal? (days-in-month 10 2001) 31 "Oct non-leap")
      (check-equal? (days-in-month 11 2001) 30 "Nov non-leap")
      (check-equal? (days-in-month 12 2001) 31 "Dec non-leap")
      (check-equal? (days-in-month 1 2000) 31 "Jan leap")
      (check-equal? (days-in-month 2 2000) 29 "Feb leap")
      (check-equal? (days-in-month 3 2000) 31 "Mar leap")
      (check-equal? (days-in-month 4 2000) 30 "Apr leap")
      (check-equal? (days-in-month 5 2000) 31 "May leap")
      (check-equal? (days-in-month 6 2000) 30 "Jun leap")
      (check-equal? (days-in-month 7 2000) 31 "Jul leap")
      (check-equal? (days-in-month 8 2000) 31 "Aug leap")
      (check-equal? (days-in-month 9 2000) 30 "Sep leap")
      (check-equal? (days-in-month 10 2000) 31 "Oct leap")
      (check-equal? (days-in-month 11 2000) 30 "Nov leap")
      (check-equal? (days-in-month 12 2000) 31 "Dec leap"))
    
    (test-case "date-valid?"
      (let ([check-date
             (lambda (nanosecond second minute hour day month year tz valid?)
               (check-equal? (date-valid? (srfi:make-date nanosecond second minute hour day month year tz)) valid?))])
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
    
    (test-case "time-weekday?"
      (check-true  (time-weekday? (date->time-tai (srfi:make-date 0 0 0 12 15 06 2007 0))) "time-tai, Fri")  ; 2007-06-15 was a Friday
      (check-false (time-weekday? (date->time-tai (srfi:make-date 0 0 0 12 16 06 2007 0))) "time-tai, Sat")  ; 2007-06-16 was a Saturday
      (check-true  (time-weekday? (date->time-utc (srfi:make-date 0 0 0 12 15 06 2007 0))) "time-utc, Fri")  ; 2007-06-15 was a Friday
      (check-false (time-weekday? (date->time-utc (srfi:make-date 0 0 0 12 16 06 2007 0))) "time-utc, Sat")) ; 2007-06-16 was a Saturday
    
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
    
    (test-case "time-duration?"
      (check-equal? (map time-duration? (list (current-time time-tai)
                                              (current-time time-utc)
                                              (make-time time-duration 0 10)
                                              (current-seconds)
                                              #f))
                    (list #f #f #t #f #f)))
    
    (test-case "time->date"
      (check-not-exn (cut time->date (current-time time-tai)))
      (check-not-exn (cut time->date (current-time time-utc)))
      (check-exn exn:fail:contract? (cut time->date (make-time time-duration 0 0))))
    
    (test-case "seconds->ago-string"
      (check-equal? (seconds->ago-string test-timestamp test-timestamp) "0 seconds ago")
      (check-equal? (seconds->ago-string (- test-timestamp 1) test-timestamp) "1 second ago")
      (check-equal? (seconds->ago-string (- test-timestamp 59) test-timestamp) "59 seconds ago")
      (check-equal? (seconds->ago-string (- test-timestamp 60) test-timestamp) "1 minute ago")
      (check-equal? (seconds->ago-string (- test-timestamp 3599) test-timestamp) "59 minutes ago")
      (check-equal? (seconds->ago-string (- test-timestamp 3600) test-timestamp) "1 hour ago")
      (check-equal? (seconds->ago-string (- test-timestamp 86399) test-timestamp) "23 hours ago")
      (check-equal? (seconds->ago-string (- test-timestamp 86400) test-timestamp) "yesterday")
      (check-equal? (seconds->ago-string (- test-timestamp 172800) test-timestamp) "2 days ago"))
    
    (test-case "time->ago-string"
      (for-each (lambda (time-type)
                  (check-equal? (time->ago-string 
                                 (make-time time-type 0 test-timestamp)
                                 (make-time time-type 0 test-timestamp))
                                "0 seconds ago"
                                (format "check 1, type ~a" time-type))
                  (check-equal? (time->ago-string 
                                 (make-time time-type 0 (- test-timestamp 60))
                                 (make-time time-type 0 test-timestamp))
                                "1 minute ago"
                                (format "check 2, type ~a" time-type))
                  (check-equal? (time->ago-string 
                                 (make-time time-type 0 (- test-timestamp 3600))
                                 (make-time time-type 0 test-timestamp))
                                "1 hour ago"
                                (format "check 3, type ~a" time-type)))
                (list time-tai time-utc)))
    
    (test-case "time->ago-string with mixture of time-tai and time-utc"
      (check-not-exn (cut time->ago-string (current-time time-tai)))
      (check-not-exn (cut time->ago-string (current-time time-utc)))
      (check-not-exn (cut time->ago-string (current-time time-tai) (current-time time-tai)))
      (check-not-exn (cut time->ago-string (current-time time-utc) (current-time time-utc)))
      (check-exn exn:fail:contract? (cut time->ago-string (current-time time-tai) (current-time time-utc)))
      (check-exn exn:fail:contract? (cut time->ago-string (current-time time-utc) (current-time time-tai))))
    
    ))
