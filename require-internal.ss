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
       (member (filename-extension path) scheme-source-extensions)))

; absolute-path
(define svn-require-cache-path
  (if (getenv "SVNREQUIRECACHE")
      (let ([ans (expand-user-path (build-path (getenv "SVNREQUIRECACHE")))])
        (if (absolute-path? ans)
            (lambda () ans)
            (error "$SVNREQUIRECACHE must be absolute" (getenv "SVNREQUIRECACHE"))))
      (lambda () (error "$SVNREQUIRECACHE not specified"))))

; string natural symbol -> complete-path
(define (svn-checkout repository-url revision package-name)
  (let ([cache-dir (build-path (svn-require-cache-path) (symbol->string package-name))])
    (if (directory-exists? cache-dir)
        (let ([command-line (format "svn up -r~a ~s" revision (path->string cache-dir))])
          (unless (system command-line)
            (error "svn update failed" command-line)))
        (let ([command-line (format "svn co -r~a ~s ~s"
                                    revision
                                    repository-url
                                    (path->string cache-dir))])
          (make-directory* cache-dir)
          (unless (system command-line)
            (error "svn checkout failed" command-line))))
    cache-dir))

; Provide statements -----------------------------

(provide/contract
 [scheme-source-extensions    (listof bytes?)]
 [scheme-source-path?         (-> path? boolean?)]
 [svn-require-cache-path      (-> (and/c path? absolute-path?))]
 [svn-checkout                (-> string?
                                  (or/c natural-number/c 'head)
                                  symbol?
                                  (and/c path? absolute-path?))])

