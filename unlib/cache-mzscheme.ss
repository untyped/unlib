#lang mzscheme

(require mzlib/kw
         "base.ss"
         "cache-internal.ss")

;  (key -> value)
;  (key value -> void)
;  [#:expire (key value -> void)]
;  [#:lifetime natural]
; ->
;  cache
(define/kw (create-cache load store #:key [expire void] [lifetime 3600])
  (make-cache load store expire lifetime))

;  (key -> value)
;  (key value -> void)
;  [#:expire (key value -> void)]
;  [#:lifetime natural]
; ->
;  cacheeq
(define/kw (create-cacheeq load store #:key [expire void] [lifetime 3600])
  (make-cacheeq load store expire lifetime))

; Provide statements -----------------------------

(provide (rename create-cache make-cache)
         (rename create-cacheeq make-cacheeq)
         cache-ref
         cache-set!
         cache-clear!
         cache-clean!
         start-timer
         stop-timer)
