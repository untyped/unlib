(module symbol mzscheme
  
  (require (lib "string.ss" "srfi" "13")
           (file "base.ss"))
  
  (provide symbol-append
           symbol-upcase
           symbol-downcase)
  
  ;; symbol-append : (U symbol string integer boolean) ... -> symbol
  (define (symbol-append . args)
    (string->symbol 
     (string-concatenate 
      (map (lambda (item)
             (cond
               [(symbol? item) (symbol->string item)]
               [(string? item) item]
               [(number? item) (number->string item)]
               [(boolean? item) (if item "true" "false")]
               [else
                (raise-exn exn:fail:unlib
                  (format "Expected (list-of (U symbol string number boolean)). Received: ~a" args))]))
           args))))
  
  ;; symbol-upcase : symbol -> symbol
  (define (symbol-upcase sym)
    (string->symbol (string-upcase (symbol->string sym))))
  
  ;; symbol-downcase : symbol -> symbol
  (define (symbol-downcase sym)
    (string->symbol (string-downcase (symbol->string sym))))
  
  )