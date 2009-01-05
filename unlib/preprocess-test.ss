#lang mzscheme

(require (file "preprocess.ss")
         (file "test-base.ss"))

(provide preprocess-tests)

(define preprocess-tests
  (test-suite "preprocess.ss"
    
    (test-case "Applying a template returns the expected result"
      (let ((tmpl (open-input-string "<< (+ 1 2) >>")))
        (check string=?
               (apply-template tmpl '())
               "3\n")))
    
    (test-case "Template bindings don't affect the current environment"
      (let ((dummy #f)
            (tmpl (open-input-string "<< (set! dummy 2) dummy >>")))
        (check string=?
               (apply-template tmpl '((dummy . 3)))
               "2\n")
        (check-equal? dummy #f)))
    
    (test-case "Template bindings are passed to the template"
      (let ((tmpl (open-input-string "<< dummy >>")))
        (check string=?
               (apply-template tmpl '((dummy . 3)))
               "3\n")))
    
    ))
