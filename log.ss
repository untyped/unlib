#lang scheme/base

(require scheme/contract
         (except-in srfi/1/list any)
         srfi/13/string
         srfi/19/time
         srfi/26/cut
         (planet schematics/macro/aif)
         (file "base.ss")
         (file "parameter.ss")
         (file "time.ss"))

; Log streams ------------------------------------

; (struct symbol)
(define-struct log-stream (name) #:transparent)

; symbol -> log-stream
(define make-log make-log-stream)

; log-stream
(define message-log (make-log 'M))
(define warning-log (make-log 'W))
(define error-log   (make-log 'E))

; (parameter (-> list))
;
; Thunk which returns a list of values to be included at the beginning of each log message.
(define-parameter current-log-preamble
  (lambda () null)
  (lambda (val)
    (if (procedure? val)
        val
        (raise-exn exn:fail:contract
          (format "Expected (symbol -> (listof string)), received ~a." val))))
  with-log-preamble)

; (parameter (U output-port (-> output-port)))
;
; A parameter controlling the output port to print messages to. The value
; can be an output port or a thunk returning an output port.
(define-parameter current-log-port
  current-output-port
  (make-guard (lambda (x) 
                (or (output-port? x) 
                    (procedure? x)))
              "(U output-port (-> output-port))")
  with-log-port)

; any ... -> integer
;
; Prints a message and returns a timestamp as a unique identifier.
(define log-message
  (lambda args
    (log-generic message-log args)))

; any ... -> integer
;
; Prints a warning and returns a timestamp as a unique identifier.
(define log-warning
  (lambda args
    (log-generic warning-log args)))

; any ... -> integer
;
; Prints a error and returns a timestamp as a unique identifier.
(define log-error
  (lambda args
    (log-generic error-log args)))

; log-stream list -> time-tai
;
; Prints a generic message and returns a timestamp as a unique identifier.
(define (log-generic log-stream message-components)
  (let* ([time  (current-time time-tai)]
         [items (cons (log-stream-name log-stream)
                      (append ((current-log-preamble)) message-components))]
         [out   (current-log-port-ref)])
    (display (string-join (map (cut format "~s" <>) items) ",") out)
    (newline out)
    time))

; Helpers ----------------------------------------

; -> output-port
(define (current-log-port-ref)
  (define port+thunk (current-log-port))
  (if (output-port? port+thunk)
      port+thunk
      (port+thunk)))

; Provide statements -----------------------------

(provide with-log-preamble
         with-log-port)

(provide/contract
 [struct log-stream    ([name symbol?])]
 [make-log             (-> symbol? log-stream?)]
 [message-log          log-stream?]
 [warning-log          log-stream?]
 [error-log            log-stream?]
 [current-log-preamble (parameter/c procedure?)]
 [current-log-port     (parameter/c (or/c output-port? (-> output-port?)))]
 [log-message          (->* () () #:rest any/c time-tai?)]
 [log-warning          (->* () () #:rest any/c time-tai?)]
 [log-error            (->* () () #:rest any/c time-tai?)]
 [log-generic          (-> log-stream? list? time-tai?)])
