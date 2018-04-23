(define-module (time)
  #:use-module (srfi srfi-19)            ; Time/Date library.
  #:use-module (macros arrow)
  #:use-module (obj)
  #:use-module (util)

  #:export (drop-time
            drop-zone-offset
            event->time
            event-date
            event-time
            vevent->time
            string->date*
            date->decimal-hour
            time->decimal-hour
            date->decimal-hour))

(define (drop-time obj)
  "Removes everything from hour and down from a date object"
  (let ((nsecs 0)
        (seconds 0)
        (minutes 0)
        (hours 0)
        (date (date-day obj))
        (month (date-month obj))
        (year (date-year obj))
        (zone-offset (date-zone-offset obj)))
    (make-date nsecs seconds minutes hours date month year zone-offset)))

(define (drop-zone-offset obj)
  "Removes zone offset from a date object"
  (let ((nsecs (date-nanosecond obj))
        (seconds (date-second obj))
        (minutes (date-minute obj))
        (hours (date-hour obj))
        (date (date-day obj))
        (month (date-month obj))
        (year (date-year obj))
        (zone-offset 0))
    (make-date nsecs seconds minutes hours date month year zone-offset)))

;;; Like the regular string->date, but specialized for my dates.  
(define (string->date* str)
  (test-until-success
   'misc-error
   (string->date str "~Y~m~dT~H~M~S~z") ; UTC-time
   (string->date str "~Y~m~dT~H~M~S")   ; Local time
   (string->date str "~Y~m~d")          ; All day
   (make-date 0 0 0 0 0 0 0 0)))

(define (event->time ev)
  (-> ev
      ((extract "DTSTART"))
      string->date*
      ;; drop-time
      date->time-utc
      ))

(define (event-date ev)
  (-> ev
      ((extract "DTSTART"))
      string->date*
      drop-time
      ))

;;; TODO
;;; This currently only works for events where DTSTART
;;; contains a time along with it's date, it needs to be
;;; a lot more general in the future.
(define (event-time ev)
  "Returns a time object matching the time of the event.
Currently does't check timezones, and assumes the current one"
  (-> ev
      ((extract "DTSTART"))
      string->date*
      date->time-utc))

(define (vevent->time field vev)
  (-> vev
      ((extract field))
      string->date*
      date->time-utc))

(define (date->decimal-hour date)
  "Coverts an SRFI-19 date objects hour and minute field
to a number between 0 and 24"
  (+ (date-hour date)
     (/ (date-minute date)
        60)))

(define (time->decimal-hour time)
  "This should only be used on time intervals,
never on absolute times. For that see date->decimal-hour"
  (exact->inexact (/ (time-second time)
                     3600)))

(define (date->decimal-hour date)
  (exact->inexact
   (+ (date-hour date)
      (/ (date-minute date)
         60))))
