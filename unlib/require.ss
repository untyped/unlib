#lang scheme/base

(require (for-syntax scheme/base
                     scheme/path
                     srfi/26/cut
                     (file "debug.ss"))
         scheme/require-syntax)

; Helpers ----------------------------------------

; (listof bytes)
(define-for-syntax scheme-source-extensions
  '(#"ss" #"scm"))

; path -> boolean
(define-for-syntax (scheme-source-path? path)
  (define extension 
    (filename-extension path))
  (and (file-exists? path) (member extension scheme-source-extensions)))

; Syntax -----------------------------------------

; (_ string)
(define-require-syntax directory-in
  (lambda (stx)
    (syntax-case stx ()
      [(_ dirname)
       (if (string? (syntax->datum #'dirname))
           (let ([path (path->complete-path (build-path (syntax->datum #'dirname)))])
             #`(combine-in #,@(map (lambda (path)
                                     #`(file #,(path->string path)))
                                   (filter scheme-source-path? 
                                           (map (cut build-path path <>)
                                                (directory-list path))))))
           (raise-syntax-error #f "directory name must be a string literal" stx #'dirname))])))

; Provide statements -----------------------------

(provide directory-in)
