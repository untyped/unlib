#lang scheme/base

(require srfi/13
         "base.ss"
         "convert.ss")

; any -> boolean
(define (string+false? item)
  (or (string? item) (not item)))

; (U string bytes any) -> (U string any)
(define (ensure-string str)
  (if (bytes? str)
      (bytes->string/utf-8 str)
      str))

; natural -> contract
(define (string-length/c num)
  (flat-named-contract
   (format "(string-length/c ~a)" num)
   (lambda (item)
     (and (string? item)
          (<= (string-length item) num)))))

; (listof string) string [#:prefix string] [#:suffix string] -> string
(define (string-delimit items delimiter #:prefix [prefix #f] #:suffix [suffix #f])
  (define delimited (string-join items delimiter))
  (if prefix
      (if suffix
          (string-append prefix delimited suffix)
          (string-append prefix delimited))
      (if suffix
          (string-append delimited suffix)
          delimited)))

; integer
(define MAX-TAB-NAME-CHARS 20)

; Tab-name trimming and ellipsising --------------

; string [integer] [string] -> string
(define (string-ellipsify str [max-length 20] [ellipsis "..."])
  ; natural
  (define str-length
    (string-length str))
  ; natural
  (define ellipsis-length
    (string-length ellipsis))
  ; string
  (cond [(<= str-length ellipsis-length) str]
        [(<= max-length ellipsis-length) str]
        [(> str-length max-length) 
         (let ([trim-length (- max-length ellipsis-length)])
           (string-append (string-trim-right (string-take str trim-length)) ellipsis))]
        [else str]))

; Provide statements ---------------------------

(provide symbol+false->string+false
         string+false->symbol+false
         string+false->number+false
         number+false->string+false)

(provide/contract
 [string+false?    procedure?]
 [ensure-string    procedure?]
 [string-length/c  (-> natural-number/c flat-contract?)]
 [string-delimit   (->* ((listof string?) string?) (#:prefix (or/c string? false/c) #:suffix (or/c string? false/c)) string?)]
 [string-ellipsify (->* (string?) (integer? string?) string?)])
