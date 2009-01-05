(module log-test mzscheme
  
  (require (file "log.ss")
           (file "test-base.ss"))
  
  (provide log-tests)

  (define log-tests
    (test-suite
     "log.ss"

     (test-case
       "log-generic produces a simple message"
       (let ([out (open-output-string)])
         (with-log-port out
           (log-generic (make-log 'X) (list "a" "b" "c" 1 2 3 #t #f)))
         (check-equal?
          (get-output-string out)
          "X,\"a\",\"b\",\"c\",1,2,3,#t,#f\n")))

      (test-case
       "log-message produces a simple message"
       (let ([out (open-output-string)])
         (with-log-port out
           (log-message "a" "b" "c" 1 2 3 #t #f))
         (check-equal?
          (get-output-string out)
          "M,\"a\",\"b\",\"c\",1,2,3,#t,#f\n")))
      
      (test-case
       "log-warning produces a simple message"
       (let ([out (open-output-string)])
         (with-log-port out
           (log-warning "a" "b" "c" 1 2 3 #t #f))
         (check-equal?
          (get-output-string out)
          "W,\"a\",\"b\",\"c\",1,2,3,#t,#f\n")))
      
      (test-case
       "log-error produces a simple message"
       (let ([out (open-output-string)])
         (with-log-port out
           (log-error "a" "b" "c" 1 2 3 #t #f))
         (check-equal?
          (get-output-string out)
          "E,\"a\",\"b\",\"c\",1,2,3,#t,#f\n")))

      (test-case
       "setting current-log-preamble adds printout to the beginning of a message"
       (let ([out (open-output-string)])
         (with-log-preamble
             (lambda ()
               (list "x" "y" "z" 1 2 3 #t #f))
           (with-log-port out
             (log-message "a" "b" "c" 1 2 3 #t #f)))
         (check-equal?
          (get-output-string out)
          "M,\"x\",\"y\",\"z\",1,2,3,#t,#f,\"a\",\"b\",\"c\",1,2,3,#t,#f\n")))

    ))

  )
