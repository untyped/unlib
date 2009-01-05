(module make-web-server mzscheme
  
  (require (file "../preprocess.ss"))
  
  ; Procedures -----------------------------------
  
  ;; make-web-server : string string (list-of (list string string string)) -> string
  ;;
  ;; Make all the Apache configuration files for a box that only hosts projects that forward to the PLT web server
  (define (make-web-server server-name server-admin sites)
    (cons
     (make-main server-name server-admin)
     (map
      (lambda (params) (apply make-site params))
      sites)))
     
  ;; make-main : string string -> string
  ;;
  ;; Make the main Apache configuration file for a box that only hosts projects that forward to the PLT web server
  (define (make-main server-name server-admin)
    (apply-template
     (open-input-file "main-apache.conf")
     `((server-name . ,server-name)
       (server-admin . ,server-admin))))
  
  ;; make-site : string string string -> string
  ;;
  ;; Make the Apache configuration file for a site that forwards to the PLT web server
  (define (make-site server-root servlet-path forward-root)
    (apply-template
     (open-input-file "site-apache.conf")
     `((server-root . ,server-root)
       (servlet-path . ,servlet-path)
       (forward-root . ,forward-root))))

  ; Provide statements --------------------------- 
  
  (provide (all-defined))
  
  )