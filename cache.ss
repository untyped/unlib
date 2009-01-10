#lang scheme/base

(require scheme/contract
         (file "cache-internal.ss"))

;  (key -> value)
;  (key value -> void)
;  [#:expire (key value -> void)]
;  [#:lifetime natural]
; ->
;  cache
(define (create-cache load store #:expire [expire void] #:lifetime [lifetime 3600])
  (make-cache load store expire lifetime))

;  (key -> value)
;  (key value -> void)
;  [#:expire (key value -> void)]
;  [#:lifetime natural]
; ->
;  cacheeq
(define (create-cacheeq load store #:expire [expire void] #:lifetime [lifetime 3600])
  (make-cacheeq load store expire lifetime))

; Provide statements -----------------------------

(provide (except-out (all-from-out (file "cache-internal.ss")) make-cache make-cacheeq))

(provide/contract 
 [rename create-cache make-cache     (->* (procedure? procedure?) (#:expire procedure? #:lifetime (>/c 0)) cache?)]
 [rename create-cacheeq make-cacheeq (->* (procedure? procedure?) (#:expire procedure? #:lifetime (>/c 0)) cache?)])