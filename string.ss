(module string mzscheme
  
  (require (lib "contract.ss")
           (lib "kw.ss")
           (lib "pregexp.ss")
           (lib "string.ss" "srfi" "13"))
  
  ;; string-namecase : string -> string
  ;;
  ;; Similar to string-titlecase, but deals with various special cases:
  ;;
  ;;     MACDONALD      => MacDonald
  ;;     MCPHERSON      => McPherson
  ;;     O'LEARY        => O'Leary
  ;;     LE COMBER      => le Comber
  ;;     VAN DER GIEZEN => van der Giezen
  (define (string-namecase str)
    (let* ([ans (string-titlecase str)]
           [do-correction
            (lambda (correct! positions offset)
              (if positions
                  (for-each
                   (lambda (pair)
                     (if pair
                         (let ([pos-to-correct (+ (cdr pair) offset)])
                           (correct! ans pos-to-correct (add1 pos-to-correct)))))
                   positions)))])
      (do-correction string-upcase!   (pregexp-match-positions "^Mac" ans) 0)
      (do-correction string-upcase!   (pregexp-match-positions " Mac" ans) 0)
      (do-correction string-upcase!   (pregexp-match-positions "^Mc"  ans) 0)
      (do-correction string-upcase!   (pregexp-match-positions " Mc"  ans) 0)
      (do-correction string-upcase!   (pregexp-match-positions "^O'"  ans) 0)
      (do-correction string-upcase!   (pregexp-match-positions " O'"  ans) 0)
      (do-correction string-downcase! (pregexp-match-positions "^Von" ans) -3)
      (do-correction string-downcase! (pregexp-match-positions " Von" ans) -3)
      (do-correction string-downcase! (pregexp-match-positions "^Van" ans) -3)
      (do-correction string-downcase! (pregexp-match-positions " Van" ans) -3)
      (do-correction string-downcase! (pregexp-match-positions "^Der" ans) -3)
      (do-correction string-downcase! (pregexp-match-positions " Der" ans) -3)
      (do-correction string-downcase! (pregexp-match-positions "^Le"  ans) -2)
      (do-correction string-downcase! (pregexp-match-positions " Le"  ans) -2)
      ans))

  ;; ensure-string : (U string bytes any-other) -> (U string any-other)
  (define (ensure-string str)
    (cond [(string? str) str]
          [(bytes? str)  (bytes->string/utf-8 str)]
          [else          str]))
  
  ;; string-delimit : (list-of string) string [#:prefix string] [#:suffix string] -> string
  (define string-delimit
    (lambda/kw (items delimiter #:key [prefix #f] [suffix #f])
      (let ([delimited (string-join items delimiter)])
        (if prefix
            (if suffix
                (string-append prefix delimited suffix)
                (string-append prefix delimited))
            (if suffix
                (string-append delimited suffix)
                delimited)))))
  
  ; Provide statements ---------------------------

  (provide string-delimit
           ensure-string)
  
  (provide/contract
   [string-namecase (-> string? string?)])
   
  )
