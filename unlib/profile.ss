(module profile mzscheme
  
  (require (lib "contract.ss")
           (file "log.ss"))

  ; Procedures -----------------------------------

  ;; profile : string (any ... -> any1) any ... -> any1  
  (define (profile message fn . args)
    (define start-time #f)
    (define end-time   #f)
    (dynamic-wind
     (lambda ()
       (set! start-time (current-milliseconds)))
     (lambda ()
       (apply fn args))
     (lambda ()
       (set! end-time (current-milliseconds))
       (log-message "Profile" message (- end-time start-time)))))
  
  ; Provide statements ---------------------------
  
  (provide/contract
   [profile (->* (string? procedure?) any/c any)])
  
  )
