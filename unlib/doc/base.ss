#lang scheme/base

(require (for-syntax scheme/base)
         scribble/eval
         scribble/manual
         (for-label scheme/base
                    (file "../cache.ss")
                    (file "../contract.ss")
                    (file "../debug.ss")
                    (file "../exn.ss")
                    (file "../file.ss")
                    (file "../gen.ss")
                    (file "../generator.ss")
                    (file "../hash-table.ss")
                    (file "../hash.ss")
                    (file "../lifebox.ss")
                    (file "../list.ss")
                    (file "../log.ss")
                    (file "../number.ss")
                    (file "../parameter.ss")
                    (file "../pipeline.ss")
                    (file "../preprocess.ss")
                    (file "../profile.ss")
                    (file "../project.ss")
                    (file "../string.ss")
                    (file "../symbol.ss")
                    (file "../syntax.ss")
                    (file "../time.ss")
                    (file "../trace.ss")
                    (file "../yield.ss")))

; Sandbox evaluation for examples ----------------

; syntax (_ id requre-spec ...) -> eval
(define-syntax (define-eval stx)
  (syntax-case stx ()
    [(_ id require-stmt ...)
     #'(define id
         (let ([id (make-base-eval)])
           (interaction-eval 
            #:eval id 
            (begin (require scheme/pretty require-stmt ...)
                   (pretty-print-columns 40)))
           id))]))

; Provide statements -----------------------------

(provide (all-from-out scribble/eval
                       scribble/manual)
         define-eval
         (for-label (all-from-out scheme/base
                                  (file "../cache.ss")
                                  (file "../contract.ss")
                                  (file "../debug.ss")
                                  (file "../exn.ss")
                                  (file "../file.ss")
                                  (file "../gen.ss")
                                  (file "../generator.ss")
                                  (file "../hash-table.ss")
                                  (file "../hash.ss")
                                  (file "../lifebox.ss")
                                  (file "../list.ss")
                                  (file "../log.ss")
                                  (file "../number.ss")
                                  (file "../parameter.ss")
                                  (file "../pipeline.ss")
                                  (file "../preprocess.ss")
                                  (file "../profile.ss")
                                  (file "../project.ss")
                                  (file "../string.ss")
                                  (file "../symbol.ss")
                                  (file "../syntax.ss")
                                  (file "../time.ss")
                                  (file "../trace.ss")
                                  (file "../yield.ss"))))