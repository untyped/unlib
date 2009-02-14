#lang scheme/base

(require "base.ss")

(require preprocessor/mzpp
         (namespace-in namespace))

; Procedures -----------------------------------

; (U string port) (alistof symbol any) -> string
(define (apply-template template bindings)
  (let ((op (open-output-string)))
    (parameterize
        ((current-output-port op))
      (let ((ns (make-base-namespace)))
        (namespace-attach/require
         ns
         '(lib "mzpp.ss" "preprocessor"))
        (with-namespace
         ns
         (for-each
          (lambda (cell)
            (let ((key (car cell))
                  (val (cdr cell)))
              (eval `(define ,key ,val))))
          bindings)
         (eval `(preprocess ,template)))))
    (get-output-string op)))

; Provide statements --------------------------- 

(provide apply-template)
