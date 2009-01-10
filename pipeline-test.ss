#lang scheme/base

(require "pipeline.ss"
         "pipeline-test-data.ss"
         "test-base.ss")

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
      '(a 0 b 1))))

; Helpers ----------------------------------------

; any -> any
(define (identity x)
  x)

; Provide statements -----------------------------

(provide pipeline-tests)
