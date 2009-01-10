#lang scheme/base

(require scheme/pretty
         (file "debug.ss")
         (file "test-base.ss"))

; Helpers ----------------------------------------

(define-syntax capture-output
  (syntax-rules ()
    [(_ expr ...)
     (let ([out (open-output-string)])
       (parameterize ([current-output-port out])
         expr ... (get-output-string out)))]))

(define-syntax discard-output
  (syntax-rules ()
    [(_ expr ...)
     (let ([out (open-output-string)])
       (parameterize ([current-output-port out])
         expr ...))]))

; Test suite -----------------------------------

(define debug-tests
  (test-suite "debug.ss"
    
    (test-equal? "debug : passes value transparently"
      (discard-output (debug "Message" (+ 1 2 3)))
      6)
    
    (test-equal? "debug : prints value"
      (capture-output (debug "Message" (+ 1 2 3)))
      "Message:\n  6\n")
    
    (test-equal? "debug* : passes value transparently"
      (discard-output (debug* "Message" + 1 2 3))
      6)
    
    (test-equal? "debug* : prints value"
      (capture-output (debug* "Message" + 1 2 3))
      "Message:\n  6\n")
    
    (test-equal? "debug-enabled?"
      (parameterize ([debug-enabled? #f])
       (capture-output (debug "Message" (+ 1 2 3))))
      "")
    
    (test-equal? "define-debug"
      (capture-output (define-debug a 2) (void))
      "a:\n  2\n")
    
    (test-equal? "let-debug"
      (capture-output (let-debug ([a 1] [b 2])
                                 (+ a b)))
      "a:\n  1\nb:\n  2\n")
    
    (test-equal? "let*-debug"
      (capture-output (let*-debug ([a 1] [b (+ a 2)])
                                  (+ a b)))
      "a:\n  1\nb:\n  3\n")
    
    (test-equal? "letrec-debug"
      (capture-output (letrec-debug ([a 1] [b 2] [c 3])
                                    (+ a b c)))
      "a:\n  1\nb:\n  2\nc:\n  3\n")
    
    (test-case "with-pretty-indent"
      (parameterize ([pretty-print-columns 6])
        (check-equal? (pretty-format (list 1 2 3 4 5))
                      "(1\n 2\n 3\n 4\n 5)")
        (check-equal? (with-pretty-indent "=="
                        (pretty-format (list 1 2 3 4 5)))
                      "==(1\n== 2\n== 3\n== 4\n== 5)")
        (check-equal? (with-pretty-indent "==="
                        (pretty-format (list 1 2 3 4 5)))
                      "===(1\n=== 2\n=== 3\n=== 4\n=== 5)")))
    
    (test-case "exn-context"
      (let ([ctxt (exn-context (make-exn "Test" (current-continuation-marks)))])
        (check-pred list? ctxt)
        (check-true (andmap symbol? ctxt))))))

; Provide statements -----------------------------

(provide debug-tests)
