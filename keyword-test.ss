#lang scheme/base

(require "keyword.ss"
         "test-base.ss")

; Helpers ----------------------------------------

(define (test-proc #:a a #:b [b 100] c [d 200] . rest)
  (list (list a b)
        (list c d)
        rest))

; Procedures -------------------------------------

(define keyword-tests
  (test-suite "keyword.ss"
    
    (test-case "keyword-apply* : null rest argument"
      (check-equal? (keyword-apply* test-proc '#:a 1 2 null)
                    (list (list 1 100)
                          (list 2 200)
                          null))
      (check-equal? (keyword-apply* test-proc '#:a 1 '#:b 2 3 4 null)
                    (list (list 1 2)
                          (list 3 4)
                          null)))
    
    (test-case "keyword-apply* : non-null rest argument"
      (check-equal? (keyword-apply* test-proc '(#:a 1 2))
                    (list (list 1 100)
                          (list 2 200)
                          null))
      (check-equal? (keyword-apply* test-proc '(#:a 1 #:b 2 3 4))
                    (list (list 1 2)
                          (list 3 4)
                          null)))
    
    (test-case "keyword-apply* : error cases"
      (check-exn exn:fail:contract? (cut keyword-apply* null))
      (check-exn exn:fail:contract? (cut keyword-apply* test-proc))
      (check-exn exn:fail:contract? (cut keyword-apply* test-proc null))
      (check-exn exn:fail:contract? (cut keyword-apply* test-proc '#:a 1 null))
      (check-exn exn:fail:contract? (cut keyword-apply* test-proc 2 null))
      (check-not-exn (cut keyword-apply* test-proc '#:a 1 2 null))
      (check-exn exn:fail:contract? (cut keyword-apply* test-proc '#:a 1 2)))))

; Provide statements -----------------------------

(provide keyword-tests)