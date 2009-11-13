#lang scheme/base

(require (for-syntax scheme/base)
         "trace.ss"
         "test-base.ss")

; Utility procedures ---------------------------

(define-syntax (capture-output stx)
  (syntax-case stx ()
    [(_ expr ...)
     #'(let ([out (open-output-string)]
             [thunk (lambda () expr ...)])
         (parameterize ([current-output-port out])
           (thunk)
           (get-output-string out)))]))

; Test data ------------------------------------

(define-traced (foo a b) (+ a b))
(define-traced bar (lambda (a b) (+ a b)))
(define-traced baz (lambda ([a 1] [b 2]) (+ a b)))

(define quux (lambda-traced (a b) (+ a b)))

; Test suite -----------------------------------

(define/provide-test-suite trace-tests
  
  (test-case "define-traced function prints entry and exit"
    (check string=?
           (capture-output (foo 1 2))
           "> (foo 1 2)\n< (foo 1 2)\n"))
  
  (test-case "define-traced lambda prints entry and exit"
    (check string=?
           (capture-output (bar 1 2))
           "> (bar 1 2)\n< (bar 1 2)\n"))
  
  (test-case "define-traced opt-lambda prints entry and exit"
    (check string=?
           (capture-output (baz))
           "> (baz 1* 2*)\n< (baz 1* 2*)\n"))
  
  (test-case "lambda-traced prints entry and exit"
    (check string=?
           (capture-output (quux 1 2))
           "Entering traced lambda:\n  (1 2)\nLeaving traced lambda:\n  3\n")))
