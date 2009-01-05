#lang scheme/base

(require (file "debug.ss")
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
    
    ))

; Provide statements -----------------------------

(provide debug-tests)
