(module pipeline mzscheme
  
  (require (lib "list.ss" "srfi" "1")
           (file "base.ss"))
  
  ;; A "pipeline" allows a programmer to wrap a procedure in one or more pieces of useful
  ;; functionality. Pipelines are lists of "stages", each of which performs some function
  ;; and calls the next stage. The last stage calls the target procedure.
  ;; 
  ;; An example of this (and the original reason for creating pipelines) is request
  ;; processing in a web server. The server may consist of a number of controller
  ;; procedures, each of which serves a different page. All of these procedures will 
  ;; have one or more bits of functionality in common:
  ;;
  ;;  - set up cookies
  ;;  - identify the user's browser
  ;;  - check the user's security privileges
  ;;
  ;; Note that, while many of these functions will be common across many controllers, there
  ;; will be occasions where one controller will need to do things differently from the others.
  ;;
  ;; The items above can be implemented as stages in a request processing pipeline. A standard
  ;; pipeline can be offered site-wide, and controllers can choose to customise it where 
  ;; appropriate by adding, removing or changing stages.
  ;;
  ;; Stages are named so they can be uniquely referred to when manipulating pipelines in this
  ;; way. This has the added advantage that single stages can be extracted and run out of context
  ;; with the rest of the pipeline.
  ;; 
  ;; More formally, given a target procedure:
  ;;
  ;;     target : any ... -> any
  ;;
  ;; a pipeline is a list of stages:
  ;;
  ;;     pipeline : (list-of stage)
  ;;
  ;; where a stage is a name and a body procedure:
  ;;
  ;;     struct stage : symbol ((any ... -> any) any ... -> any)
  ;;
  ;; The body procedure takes at least one argument: a "continuation procedure" that is called 
  ;; to continue the pipeline. The arguments passed to the continuation procedure are passed
  ;; on to the next stage in the pipeline. The target procedure is considered a "pseudo stage"
  ;; that is called after all other stages.
  ;; 
  ;; Any stage can abort the pipeline simply by failing to call the continuation procedure.
  ;; It is also perfectly reasonable for stages to set up parameters, install exception handlers,
  ;; change the arguments to subsequent stages and so on.
  ;;
  ;; [DJG] Noel says this all has something to do with "equirecursive types":
  ;;
  ;;     http://en.wikipedia.org/wiki/Recursive_type#Equirecursive_types
  ;; 
  ;; but this is beyond me.
  
  ;; call-with-pipeline : pipeline (any ... -> any) any ... -> any
  ;; 
  ;; Calls a procedure via a pipeline. The result returned is either the result of the procedure
  ;; or that of the last stage invoked.
  (define (call-with-pipeline pipeline procedure . args)
    (define (pipe pipeline . args)
      (if (null? pipeline)
          (apply procedure args)
          (let ([stage (car pipeline)]
                [success 
                 (lambda args 
                   (apply pipe (cons (cdr pipeline) args)))])
            (apply stage (cons success args)))))
    (apply pipe (cons pipeline args)))
  
  ;; struct stage : symbol ((any ... -> any) any ... -> any)
  ;;
  ;; The first argument to the body procedure is *always* a continuation procedure that passes
  ;; control to the next stage in the pipeline.
  ;;
  ;; The definition of stage takes advantage of MzScheme's "structures as procedures" functionality
  ;; such that stages can be called directly as if they are procedures. For example:
  ;;
  ;;     (define my-stage
  ;;       (make-stage 'my-stage
  ;;         (lambda (continue name age)
  ;;           (printf "Hello ~a, " name)
  ;;           (continue age))))
  ;;
  ;;     (my-stage
  ;;       (lambda (age)
  ;;         (printf "you are ~a years old!" age))
  ;;       "Dave" 27))
  ;;
  ;; would print:
  ;;
  ;;     Hello Dave, you are 27 years old!
  (define-values 
    (struct:stage
     make-stage
     stage?
     stage-ref
     stage-set!)
    (make-struct-type
     'stage           ; name-symbol
     #f               ; super-struct-type
     2                ; init-field-k
     0                ; auto-field-k
     #f               ; auto-v
     null             ; prop-value-list
     (make-inspector) ; inspector-or-false
     1                ; proc-spec
     '(0)             ; immutable-k-list
     #f))             ; guard-spec
  
  ;; stage-name : stage -> symbol
  ;;
  ;; Returns the name associated with a stage.
  (define stage-name
    (make-struct-field-accessor
     stage-ref        ; accessor-proc
     0))              ; field-pos-k 
    
  ;; find-stage : (list-of stage) symbol -> (U stage #f)
  ;;
  ;; Returns the appropriately named stage in the specified pipeline,
  ;; or #f if such a stage cannot be found.
  (define (find-stage pipeline name)
    (find 
     (lambda (item)
       (eq? (stage-name item) name))
     pipeline))
  
  ;; replace-stage : (list-of stage) stage -> (list-of stage)
  ;;
  ;; Replaces the equivalently named stage in the supplied pipeline
  ;; (if such a stage can be found).
  (define (replace-stage pipeline stage)
    (map
     (lambda (item)
       (if (eq? (stage-name stage) (stage-name item))
           stage
           item))
     pipeline))
  
  ;; delete-stage : (list-of stage) symbol -> (list-of stage)
  ;;
  ;; Deletes the appropriately named stage from the supplied pipeline
  ;; (if such a stage can be found).
  (define (delete-stage pipeline name)
    (filter
     (lambda (item)
       (not (eq? name (stage-name item))))
     pipeline))

  ; Provide statements --------------------------- 
  
  (provide call-with-pipeline
           struct:stage
           make-stage
           stage?
           stage-name
           find-stage
           replace-stage
           delete-stage)
  
  )
 