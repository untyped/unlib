#lang scheme/base

(require srfi/1/list
         srfi/26/cut
         (file "../test-base.ss")
         (prefix-in c: (file "check.ss")))

; Tests ------------------------------------------

(define check-tests
  (test-suite "check.ss"
    
    ; Constructors
    
    (test-equal? "pass"
      (c:pass)
      (list (c:make-check-success "Okay" null)))
    
    (test-equal? "warn"
      (c:warn "Warning")
      (list (c:make-check-warning "Warning" null)))
    
    (test-equal? "fail"
      (c:fail "Failure")
      (list (c:make-check-failure "Failure" null)))
    
    (test-equal? "check-all"
      (c:check-all (c:pass)
                   (c:warn "Warning")
                   (c:fail "Failure"))
      (list (c:make-check-success "Okay" null)
            (c:make-check-warning "Warning" null)
            (c:make-check-failure "Failure" null)))
    
    (test-case "check-with-handlers"
      (let ([exn (make-exn "Oops!" (current-continuation-marks))])
        (check-equal?
         (c:check-with-handlers
          (lambda ()
            (c:check-all (c:pass)
                         (c:warn "Warning")
                         (c:fail "Failure"))))
         (list (c:make-check-success "Okay" null)
               (c:make-check-warning "Warning" null)
               (c:make-check-failure "Failure" null))
         "check 1")
        (check-equal?
         (c:check-with-handlers
          (lambda ()
            (raise exn)))
         (list (c:make-check-error "Exception raised" null exn))
         "check 2")))
    
    ; Accessors / mutators
    
    (test-case "check-result-exn"
      (let ([exn (make-exn "Oops!" (current-continuation-marks))])
        (check-false (c:check-result-exn (car (c:pass))) "check 1")
        (check-false (c:check-result-exn (car (c:warn "Warning"))) "check 2")
        (check-false (c:check-result-exn (car (c:fail "Failure"))) "check 3")
        (check-equal? exn (c:check-result-exn (c:make-check-error "Error" null exn)) "check 4")))
    
    ; Combinators
    
    (test-case "check-with-annotations"
      (let* ([stage1 (c:check-all (c:pass) (c:warn "Warning"))]
             [stage2 (c:check-with-annotations '((a . 1) (b . 2)) stage1)]
             [stage3 (c:check-with-annotations '((c . 3) (d . 4)) stage2)])
        (check-equal? stage1
                      (list (c:make-check-success "Okay" null)
                            (c:make-check-warning "Warning" null))
                      "check 1")
        (check-equal? stage2
                      (list (c:make-check-success "Okay" '((a . 1) (b . 2)))
                            (c:make-check-warning "Warning" '((a . 1) (b . 2))))
                      "check 2")
        (check-equal? stage3
                      (list (c:make-check-success "Okay" '((a . 1) (b . 2) (c . 3) (d . 4)))
                            (c:make-check-warning "Warning" '((a . 1) (b . 2) (c . 3) (d . 4))))
                      "check 3")))
    
    (test-case "check-with-annotations overwrites the correct keys"
      (let ([result (car (c:check-with-annotations
                          '((b . 5) (d . 6) (e . 7))
                          (c:check-with-annotations 
                           '((a . 1) (b . 2) (c . 3) (d . 4))
                           (c:pass))))])
        (check-equal? (c:check-result-annotations result)
                      '((a . 1) (b . 5) (c . 3) (d . 6) (e . 7)))))
    
    (test-case "check-problems? works as expected"
      (check-true  (c:check-problems? (c:check-all (c:pass) (c:warn "Dang"))))
      (check-false (c:check-problems? (c:check-all (c:pass) (c:pass)))))
    
    (test-case "check-results->problems works as expected"
      (let ([exn (make-exn "Dang" (current-continuation-marks))])
        (check-equal? (c:check-results->problems
                       (c:check-all (c:pass)
                                    (c:warn "w1")
                                    (c:fail "f1")
                                    (list (c:make-check-error "e1" null exn))
                                    (c:pass)
                                    (c:warn "w2")
                                    (c:fail "f2")
                                    (list (c:make-check-error "e2" null exn))))
                      (c:check-all (list (c:make-check-error "e1" null exn))
                                   (list (c:make-check-error "e2" null exn))
                                   (c:fail "f1")
                                   (c:fail "f2")
                                   (c:warn "w1")
                                   (c:warn "w2")))))
    
    (test-case "check-results->warnings+failures+errors works as expected"
      (let*-values ([(exn) (make-exn "Dang" (current-continuation-marks))]
                    [(warnings failures errors)
                     (c:check-results->warnings+failures+errors 
                      (c:check-all (c:pass)
                                   (c:warn "w1")
                                   (c:fail "f1")
                                   (list (c:make-check-error "e1" null exn))
                                   (c:pass)
                                   (c:warn "w2")
                                   (c:fail "f2")
                                   (list (c:make-check-error "e2" null exn))))])
        (check-equal? warnings 
                      (c:check-all (c:warn "w1")
                                   (c:warn "w2"))
                      "check 1")
        (check-equal? failures
                      (c:check-all (c:fail "f1")
                                   (c:fail "f2"))
                      "check 2")
        (check-equal? errors
                      (c:check-all (list (c:make-check-error "e1" null exn))
                                   (list (c:make-check-error "e2" null exn)))
                      "check 3")))
    
    ; Annotations
    
    (test-case "check-result-annotation?"
      (let ([result (car (c:check-with-annotations
                          '((note1 . "Passed"))
                          (c:pass)))])
        (check-true (c:check-result-annotation? result 'note1) "check 1")
        (check-false (c:check-result-annotation? result 'note2) "check 2")))
    
    (test-case "check-result-annotation"
      (let ([result (car (c:check-with-annotations
                          '((note1 . "Passed"))
                          (c:pass)))])
        (check-equal? (c:check-result-annotation result 'note1) 
                      "Passed"
                      "check 1")
        (check-exn exn:fail?
          (cut c:check-result-annotation result 'note2)
          "check 2")))
    
    (test-case "check-result-annotation/default"
      (let ([result (car (c:check-with-annotations
                          '((note1 . "Passed"))
                          (c:pass)))])
        (check-equal? (c:check-result-annotation/default result 'note1 #f) 
                      "Passed" 
                      "check 1")
        (check-false (c:check-result-annotation/default result 'note2 #f)
                     "check 2")))
    
    (test-case "annotate-check-result"
      (let ([result (c:annotate-check-result (car (c:fail "F")) '((note1 . "Failed")))])
        (check-equal? (c:check-result-annotation result 'note1) "Failed")))
    
    (test-case "check-with-annotations overwrites the correct keys"
      (let ([result (c:annotate-check-result
                     (c:annotate-check-result
                      (car (c:pass))
                      '((a . 1) (b . 2) (c . 3) (d . 4)))
                     '((b . 5) (d . 6) (e . 7)))])
        (check-equal? (c:check-result-annotations result)
                      '((a . 1) (b . 5) (c . 3) (d . 6) (e . 7)))))
    
    (test-case "check-until-problems"
      (let ([stage-reached #f])
        (c:check-until-problems (lambda () (set! stage-reached 0) (c:pass))
                                (lambda () (set! stage-reached 1) (c:pass))
                                (lambda () (set! stage-reached 2) (c:pass)))
        (check-equal? stage-reached 2 "check 1"))
      (let ([stage-reached #f])
        (c:check-until-problems (lambda () (set! stage-reached 0) (c:pass))
                                (lambda () (set! stage-reached 1) (c:warn "Dang"))
                                (lambda () (set! stage-reached 2) (c:pass)))
        (check-equal? stage-reached 1 "check 2")))
    
    ))

; Provide statements -----------------------------

(provide check-tests)