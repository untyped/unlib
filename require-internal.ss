#lang scheme/base

(require scheme/file
         scheme/path
         scheme/system
         "base.ss")

; (listof bytes)
(define scheme-source-extensions
  '(#"ss" #"scm"))

; path -> boolean
(define (scheme-source-path? path)
  (and (file-exists? path)
       (member (filename-extension path) scheme-source-extensions)
       #t))

; Provide statements -----------------------------

(provide/contract
 [scheme-source-extensions    (listof bytes?)]
 [scheme-source-path?         (-> path? boolean?)])

