#lang scheme/base

(require srfi/13
         "test-base.ss")

; (struct immutable-string contunation-marks any)
(define-struct (exn:test exn) (extra-arg) #:transparent)

; Tests ------------------------------------------

(define/provide-test-suite exn-tests
  
  (test-case "raise-exn raises an exception"
    (check-exn exn:fail?
      (cut raise-exn exn:fail "This is an exception.")))
  
  (test-case "raise-exn passes extra arguments to exception constructor"
    (with-handlers ([exn:test? (lambda (exn)
                                 (check-equal? (exn:test-extra-arg exn) 123))])
      (raise-exn exn:test "Message" 123)))
  
  (test-case "reraise-exn raises an exception"
    (check-exn exn:fail?
      (lambda ()
        (reraise-exn (make-exn "Boo!" (current-continuation-marks))
                     exn:fail
                     "Foo!"))))
  
  (test-case "reraise-exn includes old and new messages"
    (with-handlers 
        ([exn:fail?
          (lambda (exn)
            (let ([msg (exn-message exn)])
              (check string-contains msg "Boo!")
              (check string-contains msg "Foo!")))])
      (reraise-exn (make-exn "Boo!" (current-continuation-marks))
                   exn:fail
                   "Foo!")))
  
  (test-case "reraise-exn includes continuation marks of original exn"
    (let ([marks #f]
          [exn #f])
      (with-handlers
          ([exn?
            (lambda (e)
              (set! marks (exn-continuation-marks e))
              (set! exn e))])
        (raise (make-exn "This is an exception" (current-continuation-marks))))
      (with-handlers
          ([exn:fail?
            (lambda (e)
              (check-equal? (exn-continuation-marks e) marks))])
        (reraise-exn exn exn:fail "Foo!"))))
  
  (test-case "reraise-exn passes extra arguments to exception constructor"
    (with-handlers ([exn:test?
                     (lambda (exn)
                       (check-equal? (exn:test-extra-arg exn) 123))])
      (reraise-exn (with-handlers ([exn? (lambda (exn) exn)])
                     (raise-exn exn "Message 1"))
                   exn:test
                   "Message 2"
                   123))))
