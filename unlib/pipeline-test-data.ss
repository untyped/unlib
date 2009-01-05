(module pipeline-test-data mzscheme
  
  (require (file "base.ss")
           (file "pipeline.ss"))
  
  ; Test data ------------------------------------
  
  ; Target function
  
  (define (target arg)
    (list 'target arg))
  
  ; Basic pipeline
  
  (define stage-a
    (make-stage 
     'a
     (lambda (continue arg) 
       (cons 'a (cons arg (continue (+ arg 1)))))))
  
  (define stage-b
    (make-stage 
     'b
     (lambda (continue arg) 
       (cons 'b (cons arg (continue (+ arg 2)))))))
  
  (define stage-c
    (make-stage 
     'c
     (lambda (continue arg) 
       (cons 'c (cons arg (continue (+ arg 3)))))))
  
  ; Replacement stages
  
  (define stage-b2
    (make-stage 
     'b
     (lambda (continue arg) 
       (cons 'b (cons arg (continue (+ arg 4)))))))
  
  (define stage-b3
    (make-stage
     'b
     (lambda (continue arg)
       (cons 'b (cons arg null)))))

  ; Provide statements --------------------------- 
  
  (provide (all-defined))
  
  )