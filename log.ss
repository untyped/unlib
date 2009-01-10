#lang scheme/base

(require scheme/async-channel
         srfi/13
         srfi/19
         (file "base.ss")
         (file "number.ss")
         (file "parameter.ss")
         (file "time.ss"))

; In the type contracts below, log-level : (U 'fatal 'error 'warning 'info 'debug)

; Helpers ----------------------------------------

; log-level (U time-utc time-tai) list -> string
(define (default-log-formatter level timestamp args)
  (string-join (list* (format-log-level level)
                      (format-log-timestamp timestamp)
                      (map (cut format "~s" <>) args))
               ","))

; log-level -> string
(define (format-log-level level)
  (case level
    [(fatal)   "F"]
    [(error)   "E"]
    [(warning) "W"]
    [(info)    "I"]
    [(debug)   "D"]))

; (U time-utc time-tai) -> string
(define (format-log-timestamp timestamp)
  (cond [(time-utc? timestamp) (date->string (time-utc->date timestamp) "~Y-~m-~d ~H:~M:~S")]
        [(time-tai? timestamp) (date->string (time-utc->date timestamp) "~Y-~m-~d ~H:~M:~S")]
        [else (raise-type-error 'format-log-timestamp "(U time-utc time-tai)" timestamp)]))

; log-level string continuation-mark-set -> void
(define (default-log-handler level message marks)
  (display message (current-output-port))
  (newline))

; Configuration ----------------------------------

; logger
(define current-application-logger
  (make-parameter
   (make-logger)
   (make-guard logger? "logger")))

; (parameter (log-level time-utc list -> string))
(define current-log-formatter
  (make-parameter default-log-formatter (make-guard procedure? "(any ... -> string)")))

; log-level (log-level string continuation-mark-set -> void) -> (-> void)
(define (start-log-output level [handler default-log-handler])
  ; log-receiver
  (define receive-evt
    (make-log-receiver (current-application-logger) level))
  
  ; async-channel
  (define stop-evt
    (make-async-channel))
  
  ; -> void
  (define (print-message)
    (match (sync receive-evt stop-evt)
      [(vector level message marks)
       (handler level message marks)
       (print-message)]
      [#f (void)]))
  
  (thread print-message)
  
  ; -> void
  (cut async-channel-put stop-evt #f))

; Logging forms ----------------------------------

; (_ id log-level)
(define-syntax-rule (define-log-form id level)
  (define-syntax-rule (id arg (... ...))
    (let ([timestamp (current-time time-utc)]
          [logger    (current-application-logger)])
      (when (log-level? logger level)
        (log-message logger
                     level
                     ((current-log-formatter) level timestamp (list arg (... ...)))
                     (current-continuation-marks)))
      timestamp)))

; (_ any ...) -> time-utc
(define-log-form log-fatal* 'fatal)

; (_ any ...) -> time-utc
(define-log-form log-error* 'error)

; (_ any ...) -> time-utc
(define-log-form log-warning* 'warning)

; (_ any ...) -> time-utc
(define-log-form log-info* 'info)

; (_ any ...) -> time-utc
(define-log-form log-debug* 'debug)

; Provide statements -----------------------------

; contract
(define log-level/c
  (one-of/c 'fatal 'error 'warning 'info 'debug))

(provide log-fatal*
         log-error*
         log-warning*
         log-info*
         log-debug*)

(provide/contract
 [current-application-logger (parameter/c logger?)]
 [current-log-formatter      (parameter/c (-> log-level/c (or/c time-utc? time-tai?) list? string?))]
 [format-log-level           (-> log-level/c string?)]
 [format-log-timestamp       (-> (or/c time-utc? time-tai?) string?)]
 [start-log-output           (->* (log-level/c) ((-> log-level/c string? continuation-mark-set? void?)) (-> void?))])
