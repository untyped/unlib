#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "time"]{SRFI19 time utilities}

@(define-eval time-eval srfi/19 (planet untyped/unlib/time))

@defmodule[(planet untyped/unlib/time)]{

Utility procedures for use with SRFI 19 times and dates.

@defproc[(copy-date [date srfi:date?] 
                    [#:nanosecond  nanosecond  (U integer #f) #f]
                    [#:second      second      (U integer #f) #f]
                    [#:minute      minute      (U integer #f) #f]
                    [#:hour        hour        (U integer #f) #f]
                    [#:day         day         (U integer #f) #f]
                    [#:month       month       (U integer #f) #f]
                    [#:year        year        (U integer #f) #f]
                    [#:zone-offset zone-offset (U integer #f) #f]) srfi:date?]{
                    
Creates a copy of @scheme[date], substituting non-@scheme[#f] arguments for the values of the relevant fields.}

@defproc[(time->date [time (U time-tai? time-utc?)]) srfi:date?]{

Converts @scheme[time] to a date.}

@defproc[(time-utc? [item any]) boolean?]{

Predicate that recognises SRFI-19 @scheme[time]@schemeidfont{s} of the @scheme[time-utc] type.}

@defproc[(time-tai? [item any]) boolean?]{

Predicate that recognises SRFI-19 @scheme[time]@schemeidfont{s} of the @scheme[time-tai] type.}

@defproc[(time-duration? [item any]) boolean?]{

Predicate that recognises SRFI-19 @scheme[time]@schemeidfont{s} of the @scheme[time-duration] type.}

@defproc[(leap-year? [year integer?]) boolean?]{

Returns @scheme[#t] if @scheme[year] is a leap year.}

@defproc[(days-in-month [month integer?] [year integer? 2001]) integer?]{

Returns the number of days in @scheme[month] in @scheme[year]. @scheme[month] is numbered from 1 to 12. @scheme[year] defaults to a non-leap year if omitted.

@examples[
  #:eval time-eval
  (days-in-month 2)
  (days-in-month 2 2000)]}

@defproc[(date-valid? [date srfi:date?]) boolean?]{

Returns @scheme[#t] if @scheme[date] is a valid date (its hour, day, month and so on are all in the correct ranges).}

@defproc[(date-day-of-the-week [date date?]) (U 'mon 'tue 'wed 'thu 'fri 'sat 'sun)]{

Returns a symbol representing the day of the week on @scheme[date].}

@defproc[(date-week-day? [date date?]) boolean?]{

Returns @scheme[#t] if @scheme[date] is a Monday, Tuesday, Wednesday or Friday.}

@defproc[(time->ago-string [then (U time-tai time-utc)]
                           [now (U time-tai time-utc) (current-time (time-type then))]
                           [#:format format-string string? "~a ~a ago"]) string?]{
Given the time of an event the past (and, optionally, another argument representing the current time), returns a textual description of the time passed since the event. Raises exn:fail:unlib if @scheme[now] is before @scheme[then]. See @scheme[seconds->ago-string] for examples.

The optional @scheme[format-string] should have two wildcards in it: one for the number (1, 2, 59) and one for the unit (seconds, minutes, days). The format string is ignored if the result is @scheme["yesterday"].}

@defproc[(seconds->ago-string [secs integer?] [now integer? (current-seconds)] [#:format format-string string? "~a ~a ago"]) string?]{

Like @scheme[time->ago-string] but @scheme[then] and @scheme[now] are integer values like those output by @scheme[current-seconds].

@examples[
  #:eval time-eval
  (seconds->ago-string (- (current-seconds) 45))
  (seconds->ago-string (- (current-seconds) (* 30 60)))
  (seconds->ago-string (- (current-seconds) (* 20 60 60)))
  (seconds->ago-string (- (current-seconds) (* 25 60 60)))
  (seconds->ago-string (- (current-seconds) (* 50 60 60)))]}

@defproc[(current-year) integer?]{

Returns the current four digit year.}

@defproc[(current-time-zone-offset) integer?]{

Returns the current local time-zone offset in seconds (taking into account DST when and where appropriate).}

} @;{end defmodule}