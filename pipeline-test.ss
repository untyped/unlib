(module pipeline-test mzscheme
  
  (require (file "pipeline.ss")
           (file "pipeline-test-data.ss")
           (file "test-base.ss"))
  
  (provide pipeline-tests)
  
  (define pipeline-tests
    (test-suite
     "pipeline.ss"

      (test-equal?
       "single stage calls continue correctly"
       (stage-a target 0)
       '(a 0 target 1))
      
      (test-equal?
       "pipeline of stages passes control correctly"
       (call-with-pipeline (list stage-a stage-b stage-c) target 0)
       '(a 0 b 1 c 3 target 6))
      
      (test-equal?
       "pipeline with deleted stage passes control correctly"
       (call-with-pipeline (delete-stage (list stage-a stage-b stage-c) 'b) target 0)
       '(a 0 c 1 target 4))
      
      (test-equal?
       "pipeline with replaced stage passes control correctly"
       (call-with-pipeline (replace-stage (list stage-a stage-b stage-c) stage-b2) target 0)
       '(a 0 b 1 c 5 target 8))
      
      (test-equal?
       "pipeline with stage tat does not pass control terminates early"
       (call-with-pipeline (list stage-a stage-b3 stage-c) target 0)
       '(a 0 b 1))
      
      (test-equal?
       "find-stage retrieves a named stage correctly"
       (find-stage (list stage-a stage-b3 stage-c) 'b)
       stage-b3)

    ))

  )
