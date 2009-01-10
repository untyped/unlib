#lang scheme/base

(require net/url
         (file "base.ss")
         (file "debug.ss"))

; url -> url
(define (url-remove-params url)
  (make-url (url-scheme url)
            (url-user url)
            (url-host url)
            (url-port url)
            (url-path-absolute? url)
            (map (lambda (path/param)
                   (make-path/param (path/param-path path/param) null))
                 (url-path url))
            (url-query url)
            (url-fragment url)))

; url -> url
(define (url-local url)
  (make-url #f ; scheme
            #f ; user
            #f ; host
            #f ; port
            (url-path-absolute? url)
            (url-path url)
            (url-query url)
            (url-fragment url)))

; url -> url
(define (url-path-only url)
  (make-url #f ; scheme
            #f ; user
            #f ; host
            #f ; port
            (url-path-absolute? url)
            (url-path url)
            null
            #f))

; Provide statements -----------------------------

(provide/contract
 [url-remove-params (-> url? url?)]
 [url-local         (-> url? url?)]
 [url-path-only     (-> url? url?)])