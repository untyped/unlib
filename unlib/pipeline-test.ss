#lang scheme/base

(require (file "pipeline.ss")
         (file "pipeline-test-data.ss")
         (file "test-base.ss"))

; Tests ------------------------------------------

(define pipeline-tests
  (test-suite "pipeline.ss"
    
    (test-equal? "single stage calls continue correctly"
      (stage-a target 0)
      '(a 0 target 1))
    
    (test-equal? "pipeline of stages passes control correctly"
      (call-with-pipeline (list stage-a stage-b stage-c) target 0)
      '(a 0 b 1 c 3 target 6))
    
    (test-equal? "pipeline with stage that does not pass control terminates early"
      (call-with-pipeline (list stage-a stage-b3 stage-c) target 0)
      '(a 0 b 1))
    
    (test-equal? "find-stage retrieves a named stage correctly"
      (find-stage (list stage-a stage-b3 stage-c) 'b)
      stage-b3)
    
    (test-case "define-stage with fixed argument list"
      ; The lambda allows us to introduce a stage with define-stage:
      ((lambda ()
         (define-stage (stage continue a b c)
           (continue (map (cut * 2 <>) (list a b c))))
         (check-equal? (call-with-pipeline (list stage) identity 1 2 3)
                       (list 2 4 6))
         (check-exn exn:fail:contract:arity?
           (lambda ()
             (call-with-pipeline (list stage) identity 1 2)))
         (check-exn exn:fail:contract:arity?
           (lambda ()
             (call-with-pipeline (list stage) identity 1 2 3 4))))))
    
    (test-case "define-stage with rest argument"
      ; The lambda allows us to introduce a stage with define-stage:
      ((lambda ()
         (define-stage (stage continue . rest)
           (continue (map (cut * 2 <>) rest)))
         (check-equal? (call-with-pipeline (list stage) identity)
                       (list))
         (check-equal? (call-with-pipeline (list stage) identity 1 2)
                       (list 2 4))
         (check-equal? (call-with-pipeline (list stage) identity 1 2 3 4)
                       (list 2 4 6 8)))))
    
    ))

; Helpers ----------------------------------------

; any -> any
(define (identity x)
  x)

; Provide statements -----------------------------

(provide pipeline-tests)
