(module log mzscheme
  
  (require (lib "contract.ss")
           (all-except (lib "list.ss" "srfi" "1") any)
           (lib "string.ss"     "srfi" "13")
           (lib "time.ss"       "srfi" "19")
           (lib "cut.ss"        "srfi" "26")
           (planet "aif.ss" ("schematics" "macro.plt" 1)) 
           (file "parameter.ss")
           (file "base.ss"))
  
  (provide (all-defined))

  ;; struct log : symbol
  (define-struct log-stream (name) #f)
  (define make-log make-log-stream)
  
  (define message-log (make-log 'M))
  (define warning-log (make-log 'W))
  (define error-log   (make-log 'E))

  ;; parameter current-log-preamble : (-> (list-of any))
  ;;
  ;; Thunk which returns a list of values to be included at the beginning of each log message.
  (define-parameter current-log-preamble
    (lambda () null)
    (lambda (val)
      (if (procedure? val)
          val
          (raise-exn exn:fail:unlib
            (format "Expected (symbol -> (list-of string)), received ~a." val))))
    with-log-preamble)
  
  ;; parameter current-log-port : (U (parameter output-port) output-port)
  ;;
  ;; A parameter containing an output port or an output port
  ;; to print messages to.
  (define-parameter current-log-port
    current-output-port
    (lambda (val)
      (if (output-port? val)
          val
          (raise-exn exn:fail:unlib
            (format "Expected output-port, received ~a." val))))
    with-log-port)
  
  ;; log-message : any ... -> integer
  ;;
  ;; Prints a message and returns a timestamp as a unique identifier.
  (define log-message
    (lambda args
      (log-generic message-log args)))
  
  ;; log-warning : any ... -> integer
  ;;
  ;; Prints a warning and returns a timestamp as a unique identifier.
  (define log-warning
    (lambda args
      (log-generic warning-log args)))
  
  ;; log-error : any ... -> integer
  ;;
  ;; Prints a error and returns a timestamp as a unique identifier.
  (define log-error
    (lambda args
      (log-generic error-log args)))
  
  ;; log-generic : log (list-of any) -> time-tai
  ;;
  ;; Prints a generic message and returns a timestamp as a unique identifier.
  (define/contract log-generic
    (-> log-stream? list? any/c)
    (lambda (log-stream message-components)
      (let* ([time  (current-time time-tai)]
             [items (cons (log-stream-name log-stream)
                          (append ((current-log-preamble)) message-components))]
             [out   (aif log-port parameter? (current-log-port)
                         (log-port)
                         log-port)])
        (display (string-join (map (cut format "~s" <>) items) ",") out)
        (newline out)
        time)))
  
  )
