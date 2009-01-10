#lang scheme/base

(require srfi/26/cut
         (file "log.ss")
         (file "test-base.ss"))

; Tests ------------------------------------------

(define log-tests
  (test-suite "log.ss"
    
    (test-case "log-generic"
      (let ([out (open-output-string)])
        (with-log-port out (log-generic (make-log 'X) (list "a" "b" "c" 1 2 3 #t #f)))
        (check-equal? (get-output-string out)
                      "X,\"a\",\"b\",\"c\",1,2,3,#t,#f\n")))
    
    (test-case "log-message"
      (let ([out (open-output-string)])
        (with-log-port out (log-message "a" "b" "c" 1 2 3 #t #f))
        (check-equal? (get-output-string out)
                      "M,\"a\",\"b\",\"c\",1,2,3,#t,#f\n")))
    
    (test-case "log-warning"
      (let ([out (open-output-string)])
        (with-log-port out (log-warning "a" "b" "c" 1 2 3 #t #f))
        (check-equal? (get-output-string out)
                      "W,\"a\",\"b\",\"c\",1,2,3,#t,#f\n")))
    
    (test-case "log-error"
      (let ([out (open-output-string)])
        (with-log-port out (log-error "a" "b" "c" 1 2 3 #t #f))
        (check-equal? (get-output-string out)
                      "E,\"a\",\"b\",\"c\",1,2,3,#t,#f\n")))
    
    (test-case "current-log-port"
      (check-not-exn (cut current-log-port current-output-port))
      (check-not-exn (cut current-log-port (current-output-port)))
      (check-exn exn:fail:contract? (cut current-log-port #f)))
    
    (test-case "current-log-preamble"
      (let ([out (open-output-string)])
        (with-log-preamble
         (lambda ()
           (list "x" "y" "z" 1 2 3 #t #f))
         (with-log-port out (log-message "a" "b" "c" 1 2 3 #t #f)))
        (check-equal? (get-output-string out)
                      "M,\"x\",\"y\",\"z\",1,2,3,#t,#f,\"a\",\"b\",\"c\",1,2,3,#t,#f\n")))
    
    ))

; Provide statements -----------------------------

(provide log-tests)
