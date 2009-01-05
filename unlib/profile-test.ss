(module profile-test mzscheme
  
  (require (lib "pregexp.ss"))
  
  (require (file "log.ss")
           (file "profile.ss")
           (file "test-base.ss"))
  
  (provide profile-tests)
  
  (define profile-tests
    (test-suite
     "profile.ss"
     
     (test-case
      "profile produces a message"
      (let* ([out (open-output-string)]
             [str (with-log-port out
                    (profile "profilemessage" 
                             (lambda () 
                               (log-message "bodymessage")))
                    (get-output-string out))])
        (check-pred
         list?
         (pregexp-match #px"bodymessage.+profilemessage" str)
         (format "Output string: ~s" str))))

     (test-case
      "profile returns result of argument"
      ;; We use with-log-port to eat the output of profile,
      ;; so it doesn't appear in the test runs.  This isn't
      ;; necessary, but it makes the test output easier to
      ;; read when it isn't full of noise.
      (let ([out (open-output-string)]
            [thunk (lambda () 'foo)])
        (check-equal?
         (with-log-port out (profile "message" thunk))
         'foo)))

     (test-case
      "var-arg profile produces message"
      (let* ([out (open-output-string)]
             [str (with-log-port out
                    (profile
                     "profilemessage" 
                     (lambda (a b c) 
                       (log-message
                        (format "bodymessage ~a~a~a" a b c)))
                     1 2 3)
                    (get-output-string out))])
        (check-pred
         list?
         (pregexp-match #px"bodymessage 123.+profilemessage" str)
         (format "Output string: ~s" str))))

     (test-case
      "var-arg profile returns result of function"
      (let ([out (open-output-string)]
            [fn (lambda (a b c) (+ a b c))])
        (check-equal?
         (with-log-port out (profile "message" fn 1 2 3))
         6)))
     
     ))
  
  )
 
