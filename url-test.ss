#lang scheme/base

(require net/url
         "url.ss"
         "test-base.ss")

(define-check (check-url-proc proc actual expected)
  (check-equal? (url->string (proc (string->url actual))) expected))

; Tests ------------------------------------------

(define url-tests
  (test-suite "url.ss"
    
    (test-case "url-remove-params"
      (check-url-proc url-remove-params "/abc/def/ghi" "/abc/def/ghi")
      (check-url-proc url-remove-params "/1*2*3/4*5*6/" "/1*2*3/4*5*6/")
      (check-url-proc url-remove-params "/abc;1*2*3/def;4*5*6/" "/abc/def/")
      (check-url-proc url-remove-params "/;1*2*3" "/")
      (check-url-proc url-remove-params "http://dave@example.com:8080/abc/def/ghi?a=b&c=d#efg" "http://dave@example.com:8080/abc/def/ghi?a=b&c=d#efg")
      (check-url-proc url-remove-params "http://dave@example.com:8080/1*2*3/4*5*6/?a=b&c=d#efg" "http://dave@example.com:8080/1*2*3/4*5*6/?a=b&c=d#efg")
      (check-url-proc url-remove-params "http://dave@example.com:8080/abc;1*2*3/def;4*5*6?a=b&c=d#efg" "http://dave@example.com:8080/abc/def?a=b&c=d#efg")
      (check-url-proc url-remove-params "http://dave@example.com:8080/;1*2*3?a=b&c=d#efg" "http://dave@example.com:8080/?a=b&c=d#efg"))
    
    (test-case "url-local"
      (check-url-proc url-local "/abc/def/ghi" "/abc/def/ghi")
      (check-url-proc url-local "abc/def/ghi" "abc/def/ghi")
      (check-url-proc url-local "/abc/def/ghi;jkl" "/abc/def/ghi;jkl")
      (check-url-proc url-local "/" "/")
      (check-url-proc url-local "http://dave@example.com:8080/abc/def/ghi" "/abc/def/ghi")
      (check-url-proc url-local "http://dave@example.com:8080/abc/def/ghi?j=k#l" "/abc/def/ghi?j=k#l")
      (check-url-proc url-local "http://dave@example.com:8080/" "/"))
    
    (test-case "url-path-only"
      (check-url-proc url-path-only "/abc/def/ghi" "/abc/def/ghi")
      (check-url-proc url-path-only "abc/def/ghi" "abc/def/ghi")
      (check-url-proc url-path-only "/abc/def/ghi;jkl" "/abc/def/ghi;jkl")
      (check-url-proc url-path-only "/" "/")
      (check-url-proc url-path-only "http://dave@example.com:8080/abc/def/ghi" "/abc/def/ghi")
      (check-url-proc url-path-only "http://dave@example.com:8080/abc/def/ghi?j=k#l" "/abc/def/ghi")
      (check-url-proc url-path-only "http://dave@example.com:8080/" "/"))))
      
; Provide statements -----------------------------

(provide url-tests)
