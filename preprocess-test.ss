#lang scheme/base

(require "preprocess.ss"
         "test-base.ss")

; Tests -------------------------------------------

(define/provide-test-suite preprocess-tests
  
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
             "3\n"))))
