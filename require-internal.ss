#lang scheme/base

(require scheme/contract
         scheme/file
         scheme/path
         scheme/provide-transform
         scheme/require-transform
         scheme/system
         "profile.ss")

(define-timer require-timer)
(define-timer provide-timer)

; (listof bytes)
(define scheme-source-extensions
  '(#"ss" #"scm"))

; path -> boolean
(define (scheme-source-path? path)
  (and (file-exists? path)
       (member (filename-extension path) scheme-source-extensions)
       #t))

; Copied from scheme/require-syntax
(define (make-require-macro cert proc)
  (make-require-transformer
   (lambda (stx)
     (let* ([i (make-syntax-introducer)]
            [new-stx (cert (i (proc (i stx))) i)])
       (expand-import new-stx)))))

; Copied from scheme/provide-syntax
(define (make-provide-macro cert proc)
  (make-provide-transformer
   (lambda (stx modes)
     (let* ([i (make-syntax-introducer)]
            [new-stx (cert (i (proc (i stx))) i)])
       (expand-export new-stx modes)))))

; path -> require-transformer provide-transformer
; Acknowledgement - copied in part from cce/scheme.plt:
(define (make-file-library-transformers root-spec)
  (define root-path
    (path->complete-path (expand-user-path (build-path root-spec))))
  (define (make-path datum)
    (path->string (build-path root-path (format "~a.ss" datum))))
  (unless (directory-exists? root-path)
    (error "directory not found" (path->string root-path)))
  (values
   (make-require-transformer
    (lambda (stx)
      (syntax-case stx ()
        [(_)            (expand-import (datum->syntax stx `(file ,(make-path 'main))))]
        ;[(_ [path ...]) (andmap identifier? (syntax->list #'(path ...)))
        ;                (expand-import (datum->syntax stx `(combine-in ,@(map (lambda (stx)
        ;                                                                        `(file ,(make-path (syntax->datum stx))))
        ;                                                                      (syntax->list #'(path ...))))))]
        [(_ path)       (identifier? #'path)
                        (expand-import (datum->syntax stx `(file ,(make-path (syntax->datum #'path)))))]
        [(_ path ...)   (andmap identifier? (syntax->list #'(path ...)))
                        (expand-import (datum->syntax stx `(combine-in ,@(map (lambda (stx)
                                                                                `(file ,(make-path (syntax->datum stx))))
                                                                              (syntax->list #'(path ...))))))])))
   (make-provide-transformer
    (lambda (stx modes)
      (syntax-case stx ()
        [(_)            (expand-export (datum->syntax stx `(all-from-out (file ,(make-path 'main)))) modes)]
        ;[(_ [path ...]) (andmap identifier? (syntax->list #'(path ...)))
        ;                (expand-export (datum->syntax stx `(combine-out ,@(map (lambda (stx)
        ;                                                                         `(all-from-out (file ,(make-path (syntax->datum stx)))))
        ;                                                                       (syntax->list #'(path ...))))) modes)]
        [(_ path)       (identifier? #'path)
                        (expand-export (datum->syntax stx `(all-from-out (file ,(make-path (syntax->datum #'path)))) modes))]
        [(_ path ...)   (andmap identifier? (syntax->list #'(path ...)))
                        (expand-export (datum->syntax stx `(combine-out ,@(map (lambda (stx)
                                                                                 `(all-from-out (file ,(make-path (syntax->datum stx)))))
                                                                               (syntax->list #'(path ...))))) modes)])))))

; symbol -> require-transformer provide-transformer
; Acknowledgement - copied in part from cce/scheme.plt:
(define (make-planet-library-transformers root-spec)
  (define (make-path datum)
    (string->symbol (format "~a/~a" root-spec datum)))
  (values
   (make-require-transformer
    (lambda (stx)
      (syntax-case stx ()
        [(_)            (expand-import (datum->syntax stx `(planet ,(make-path 'main))))]
        [(_ [path ...]) (andmap identifier? (syntax->list #'(path ...)))
                        (expand-import (datum->syntax stx `(combine-in ,@(map (lambda (stx)
                                                                                `(planet ,(make-path (syntax->datum stx))))
                                                                              (syntax->list #'(path ...))))))]
        [(_ path)       (identifier? #'path)
                        (expand-import (datum->syntax stx `(planet ,(make-path (syntax->datum #'path)))))]
        [(_ path ...)   (andmap identifier? (syntax->list #'(path ...)))
                        (expand-import (datum->syntax stx `(combine-in ,@(map (lambda (stx)
                                                                                `(planet ,(make-path (syntax->datum stx))))
                                                                              (syntax->list #'(path ...))))))])))
   (make-provide-transformer
    (lambda (stx modes)
      (syntax-case stx ()
        [(_)            (expand-export (datum->syntax stx `(all-from-out (planet ,(make-path 'main)))) modes)]
        [(_ [path ...]) (andmap identifier? (syntax->list #'(path ...)))
                        (expand-export (datum->syntax stx `(combine-out ,@(map (lambda (stx)
                                                                                 `(all-from-out (planet ,(make-path (syntax->datum stx)))))
                                                                               (syntax->list #'(path ...))))) modes)]
        [(_ path)       (identifier? #'path)
                        (expand-export (datum->syntax stx `(all-from-out (planet ,(make-path (syntax->datum #'path))))) modes)]
        [(_ path ...)   (andmap identifier? (syntax->list #'(path ...)))
                        (expand-export (datum->syntax stx `(combine-out ,@(map (lambda (stx)
                                                                                 `(all-from-out (planet ,(make-path (syntax->datum stx)))))
                                                                               (syntax->list #'(path ...))))) modes)])))))

; Provide statements -----------------------------

(provide/contract
 [scheme-source-extensions         (listof bytes?)]
 [scheme-source-path?              (-> path? boolean?)]
 [make-file-library-transformers   procedure?]
 [make-planet-library-transformers procedure?])

