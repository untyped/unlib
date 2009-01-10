#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "log"]{Logging utilities}

@(define-eval log-eval (planet untyped/unlib/log))

@defmodule[(planet untyped/unlib/log)]{
                                       
This module adds some useful features to the core logging functionality of PLT Scheme:
     
@itemize{
   @item{an @italic{application log} to which application-level messages may be logged (several Untyped libraries write to this log);}
   @item{application log messages can be timestamped;}
   @item{standard debugging information can be included in application log messages;}
   @item{special application logging macros are provided that allow multiple arguments of any type.}}

Application log messages are not reported until a call is made to @scheme[start-log-output], or until a call is made to @scheme[log-receiver].

@defform[(log-fatal* any ...)]{
Adds a fatal error message to the application log. The arguments are formatted them into a single-line log message together with the message level and current time. The time is returned as a SRFI 19 @scheme[time-utc?].}
   
@defform[(log-error* any ...)]{
Adds a non-fatal error message to the application log. Behaviour is similar to that of @scheme[log-fatal*].}
   
@defform[(log-warning* any ...)]{
Adds a warning message to the application log. Behaviour is similar to that of @scheme[log-fatal*].}
   
@defform[(log-info* any ...)]{
Adds an informational message to the application log. Behaviour is similar to that of @scheme[log-fatal*].}
   
@defform[(log-debug* any ...)]{
Adds a debugging message to the application log. Behaviour is similar to that of @scheme[log-fatal*].}

@defproc[(start-log-output [level   (U 'fatal 'error 'warning 'info 'debug)]
                           [handler handler-procedure default-log-handler])
         (-> void?)]{
Starts a thread that handles application lof messages of the specified @scheme[level] or above. Returns a thunk that stops the logging thread and terminates output.

@scheme[handler] is called each time a message is logged, and should accept three arguments:
       
@itemize{
   @item{the @italic{level} of the message @scheme[(U 'fatal 'error 'warning 'info 'debug)];}
   @item{the @italic{message} to log (a @scheme[string]);}
   @item{the @italic{continuation marks} at the point of log entry (a @scheme[continuation-mark-set]).}}

If @scheme[handler] is omitted, a @scheme[default-handler-procedure] is used that prints all messages to the @scheme[current-output-port].}

@defproc*[([(current-log-formatter) (log-level time-utc? list? -> string?)]
           [(current-log-formatter [formatter (log-level time-utc? list? -> string?)]) void?])]{
Parameter that customises the formatting of application log messages. The value must be a procedure that takes three arguments:
          
@itemize{
   @item{the log level (@scheme['fatal], @scheme['error], @scheme['warning], @scheme['info] or @scheme['debug]);}
   @item{an entry timestamp (encoded as a SRFI 19 @scheme[time-utc?]);}
   @item{the list of the arguments passed to the logging macro (@scheme[log-fatal*] or equivalent).}}}

} @;{end defmodule}