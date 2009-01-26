#lang scheme/base

(require (for-syntax scheme/base
                     scheme/path
                     "base.ss"
                     "debug.ss"
                     "number.ss"
                     "require-internal.ss")
         scheme/require-syntax)

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

; (_ id relative-or-absolute-directory-string)
(define-syntax (define-file-require-syntax stx)
  (syntax-case stx ()
    [(_ id root)
     (and (identifier? #'id)
          (string? (syntax->datum #'root)))
     #'(define-require-syntax id
         (let ([root-path (path->complete-path (expand-user-path (build-path root)))])
           (unless (directory-exists? root-path)
             (error "directory not found" (path->string root-path)))
           (lambda (stx)
             (syntax-case stx ()
               [(_)             (datum->syntax stx `(file ,(path->string (build-path root-path "main.ss"))))]
               [(_ module-spec) (datum->syntax stx `(file ,(path->string (build-path root-path (format "~a.ss" (syntax->datum #'module-spec))))))]))))]))

; (_ id shorthand-planet-package-spec)
(define-syntax (define-planet-require-syntax stx)
  (syntax-case stx ()
    [(_ id package-spec)
     (and (identifier? #'id)
          (identifier? #'package-spec))
     #'(define-require-syntax id
         (lambda (stx)
           (syntax-case stx ()
             [(_)             (datum->syntax stx `(planet package-spec))]
             [(_ module-spec) (datum->syntax stx `(planet ,(string->symbol (format "~a/~a" 'package-spec (syntax->datum #'module-spec)))))])))]))

; (_ id url-string)
; (_ id url-string revision-number-or-head)
(define-syntax (define-svn-require-syntax stx)
  (syntax-case stx ()
    [(_ id repos) #'(define-svn-require-syntax id repos 'HEAD)]
    [(_ id repos revision)
     (identifier? #'id)
     (cond [(not (string? (syntax->datum #'repos)))
            (raise-syntax-error #f "repository location must be a string literal" stx #'repos)]
           [(not (or (natural? (syntax->datum #'revision))
                     (eq? (syntax->datum #'revision) 'HEAD)))
            (raise-syntax-error #f "revision must be (U natural 'head)" stx #'revision)]
           [else (let* ([repos    (syntax->datum #'repos)]
                        [revision (syntax->datum #'revision)]
                        [path     (svn-checkout repos revision (syntax->datum #'id))])
                   #`(define-file-require-syntax id #,(path->string path)))])]))

; Provide statements -----------------------------

(provide directory-in
         define-file-require-syntax
         define-planet-require-syntax
         define-svn-require-syntax)
