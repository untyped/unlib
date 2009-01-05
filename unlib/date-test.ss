(module date-test mzscheme
  
  (require (lib "time.ss" "srfi" "19")
           (lib "cut.ss"  "srfi" "26"))
  
  (require (file "date.ss")
           (file "test-base.ss"))
  
  (provide date-tests)
  
  (define test-timestamp (current-seconds))
  
  (define date-tests
    (test-suite
     "All tests for date"
     
     (test-case
      "leap-year? : 2001 is not a leap year"
      (check-equal? (leap-year? 2001) #f))
     
     (test-case
      "leap-year? : 2004 is a leap year"
      (check-equal? (leap-year? 2004) #t))
     
     (test-case
      "leap-year? : 2000 is a leap year"
      (check-equal? (leap-year? 2000) #t))
     
     (test-case
      "leap-year? : 1900 is not a leap year"
      (check-equal? (leap-year? 1900) #f))
     
     (test-case
      "days-in-month : non-leap year"
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
      (check-equal? (days-in-month 12 2001) 31))
     
     (test-case
      "days-in-month : leap year"
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
     
     (test-case
      "date-valid? : works as expected"
      (let ([check-date
             (lambda (nanosecond second minute hour day month year tz valid?)
               (check-equal? (date-valid? (make-srfi:date nanosecond second minute hour day month year tz)) valid?))])
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
     
     (test-case
      "time-weekday? : works on time-tai"
      (check-true  (time-weekday? (date->time-tai (make-srfi:date 0 0 0 12 15 06 2007 0))))  ; 2007-06-15 was a Friday
      (check-false (time-weekday? (date->time-tai (make-srfi:date 0 0 0 12 16 06 2007 0))))) ; 2007-06-16 was a Saturday
     
     (test-case
      "time-weekday? : works on time-utc"
      (check-true  (time-weekday? (date->time-utc (make-srfi:date 0 0 0 12 15 06 2007 0))))  ; 2007-06-15 was a Friday
      (check-false (time-weekday? (date->time-utc (make-srfi:date 0 0 0 12 16 06 2007 0))))) ; 2007-06-16 was a Saturday
     
     (test-case
      "time-tai? works as expected"
      (check-equal? (map time-tai? (list (current-time time-tai)
                                         (current-time time-utc)
                                         (make-time time-duration 0 10)
                                         (current-seconds)
                                         #f))
                    (list #t #f #f #f #f)))
     
     (test-case
      "time-utc? works as expected"
      (check-equal? (map time-utc? (list (current-time time-tai)
                                         (current-time time-utc)
                                         (make-time time-duration 0 10)
                                         (current-seconds)
                                         #f))
                    (list #f #t #f #f #f)))
     
     (test-case
      "time-duration? works as expected"
      (check-equal? (map time-duration? (list (current-time time-tai)
                                              (current-time time-utc)
                                              (make-time time-duration 0 10)
                                              (current-seconds)
                                              #f))
                    (list #f #f #t #f #f)))
     
     (test-case
      "time->date works as expected"
      (check-not-exn (cut time->date (current-time time-tai)))
      (check-not-exn (cut time->date (current-time time-utc)))
      (check-exn exn:fail:contract? (cut time->date (make-time time-duration 0 0))))
     
     (test-case
      "timestamp->ago-string works with an order of magnitude of seconds"
      (check-equal? (timestamp->ago-string test-timestamp test-timestamp) "0 seconds ago")
      (check-equal? (timestamp->ago-string (- test-timestamp 1) test-timestamp) "1 second ago")
      (check-equal? (timestamp->ago-string (- test-timestamp 59) test-timestamp) "59 seconds ago"))
     
     (test-case
      "timestamp->ago-string works with an order of magnitude of minutes"
      (check-equal? (timestamp->ago-string (- test-timestamp 60) test-timestamp) "1 minute ago")
      (check-equal? (timestamp->ago-string (- test-timestamp 3599) test-timestamp) "59 minutes ago"))
     
     (test-case
      "timestamp->ago-string works with an order of magnitude of hours"
      (check-equal? (timestamp->ago-string (- test-timestamp 3600) test-timestamp) "1 hour ago")
      (check-equal? (timestamp->ago-string (- test-timestamp 86399) test-timestamp) "23 hours ago"))
     
     (test-case
      "timestamp->ago-string works with an order of magnitude of days"
      (check-equal? (timestamp->ago-string (- test-timestamp 86400) test-timestamp) "yesterday")
      (check-equal? (timestamp->ago-string (- test-timestamp 172800) test-timestamp) "2 days ago"))
     
     (test-case
      "time->ago-string works as expected"
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
     
     (test-case
      "time->ago-string fails if a mixture of time-tai and time-utc are provided"
      (check-not-exn (cut time->ago-string (current-time time-tai)))
      (check-not-exn (cut time->ago-string (current-time time-utc)))
      (check-not-exn (cut time->ago-string (current-time time-tai) (current-time time-tai)))
      (check-not-exn (cut time->ago-string (current-time time-utc) (current-time time-utc)))
      (check-exn exn:fail:contract? (cut time->ago-string (current-time time-tai) (current-time time-utc)))
      (check-exn exn:fail:contract? (cut time->ago-string (current-time time-utc) (current-time time-tai))))
      
     ))
  
  )
 