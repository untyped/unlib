(module yield mzscheme
  
  (require (lib "contract.ss")
           (lib "cut.ss" "srfi" "26"))
  
  (provide yieldable)
  
  (provide/contract 
   [make-yieldable (-> procedure? procedure?)])
  
  ;; make-yieldable : (yield-procedure -> target-procedure)
  ;;               -> target-procedure
  ;;
  ;; where target-procedure and yield-procedure have symmetric
  ;; contracts:
  ;;
  ;;     target-procedure : a b c -> d e f
  ;;     yield-procedure  : d e f -> a b c
  ;;
  ;; The target procedure behaves like a normal procedure,
  ;; except that execution can be paused and resumed using the
  ;; yield procedure.
  ;;
  ;; Calling yield suspends execution of the target procedure,
  ;; and returns the values d, e and f to the caller.
  ;;
  ;; Subsequent calls to the target procedure resume execution
  ;; from the position of the last call to yield. The arguments
  ;; passed to the target are made available as the return values
  ;; of yield.
  (define (make-yieldable yield->body)
    ;; caller : (U continuation #f)
    ;; where continuation : any -> any
    (define caller #f)
    ;; resume : (U continuation #f)
    (define resume #f)
    ;; yield : d e f -> a b c
    (define (yield . args)
      (apply values 
             (let/cc k
               (set! resume k)
               (apply caller args))))
    ;; return : d e f -> a b c
    (define (return . args)
      (apply values 
             (let/cc k
               (set! resume #f)
               (apply caller args))))
    ;; body : a b c -> d e f
    (define body
      (yield->body yield))
    ; Main procedure body : a b c -> d e f
    (lambda args
      (let/cc k
        (set! caller k)
        (if resume
            (resume args)
            (call-with-values (cut apply body args)
                              return)))))

  ;; syntax (yieldable (id) stmt ...)
  (define-syntax (yieldable stx)
    (syntax-case stx ()
      [(_ yield statement ...)
       #'(make-yieldable (lambda (yield) statement ...))]))
  
  )
