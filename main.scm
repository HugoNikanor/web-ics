;;; The plan with this program is to create web based calendar
;;; viewor of a vdir directory, as created by vdirsyncer, and
;;; which can also be read by khal.
;;;
;;; Currently I'm working on getting ICS files properly read
;;; and the correct data extracted from them.

(define-module (main)
  #:use-module (ics)
  #:use-module (ics type object)
  #:use-module (ics type property property)

  #:use-module (srfi srfi-1)             ; List library.
  #:use-module (srfi srfi-19)            ; Time/Date library.
  #:use-module (srfi srfi-26)            ; Specializing parameters

  ;; These are from my guile-libs
  #:use-module (macros arrow)
  #:use-module (css)
  #:use-module (output line)

  #:use-module (oop goops)

  #:use-module (ice-9 ftw)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 format)
  ;; (ice-9 rdelim)

  #:use-module (util)

  #:export (<ics-path-object> describe-vevent get-files-in-dir slot-set-ret!
            extract filter-on-property event-time drop-time event->time
            get-rand-color date->decimal-hour time->decimal-hour vevent->time
            vev->sxml event-group->sxml get-sxml-doc
            *group-evs*))

(define-class <ics-path-object> (<ics-object>)
  (path #:getter ics-filepath))

(define (describe-vevent vev)
  (let ((props (ics-object-properties vev)))
    (string-join 
     (map (cut format #f "~10,@a: ~a" <> <>)
          (map ics-property-name props)
          (map ics-property-value props))
     "\n"
     'suffix)))

(define-method (describe (vev <ics-object>))
  (display (describe-vevent vev)))

(define-method (describe (vev <ics-path-object>))
  (display (ics-filepath vev))
  (newline)
  (next-method))

(define *cal-path*
  (string-append
   (getenv "HOME")
   "/.calendars/b85ba2e9-18aa-4451-91bb-b52da930e977/"))

;; (define (handle-ft-node node)
;;   (apply (lambda (name flags . children)
;;            (case (stat:type flags)
;;              ((directory) (map (compose (cut string-append name <>)
;;                                         handle-ft-node)
;;                                children))
;;              ((regular) name)))
;;          node))

(define (get-files-in-dir path ext)
  "Returns a list of all direct children of <path> which have
the file extension <ext>"
  (map (compose (cut string-append path <>)
                car)
       (filter (lambda (node)
                 (apply (lambda (name flags . children)
                          (string=? ext (file-extension name)))
                        node))
               (cddr (file-system-tree path)))))

(define (slot-set-ret! obj slot value)
  "Sets value of slot, and returns the object
instead of the new value"
  (slot-set! obj slot value)
  obj)

(define ((extract field) item)
  "Get value of field in item"
  (ics-property-value
   (ics-object-property-ref item field)))

;; (define (map-on-property property-name func items)
;;   "This is probably redundant."
;;   (map (compose func
;;                 (extract property-name))
;;        items))

(define (filter-on-property property-name func items)
  "Filter, but only on lists of VEVENT's, and 
\(extract property-name) is run on the item before
func recieves it."
  (filter (lambda (item)
            (func ((extract property-name) item)))
          items))


;;; TODO
;;; This currently only works for events where DTSTART
;;; contains a time along with it's date, it needs to be
;;; a lot more general in the future.
(define (event-time ev)
  "Returns a time object matching the time of the event.
Currently does't check timezones, and assumes the current one"
  (-> ev
      ((extract "DTSTART"))
      (string->date "~Y~m~dT~H~M~S")
      date->time-utc))

;;; list of all ics-objects in filename
;;; parsed as if each file only had one VEVENT
;;; origininal path of file stored in object as well
(define *ics-objs*
  (map (lambda (filename)
         (-> filename
             open-input-file
             ics->scm
             car
             ics-object-components
             car
             (change-class <ics-path-object>)
             (slot-set-ret! 'path filename)))
       (get-files-in-dir *cal-path* "ics")))


(define *limited-events*
  (filter-on-property "DTSTART"
                      (lambda (time)
                        (=  15 (string-length time)))
                      *ics-objs*))

;;; Sorted events
(define *sevs* (sort* *limited-events* time<? event-time)) 

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

(define (event->time ev)
  (-> ev
      ((extract "DTSTART"))
      (string->date "~Y~m~dT~H~M~S")
      drop-time
      date->time-utc
      ))

;;; Each element is a day, the car is a time-utc object, which
;;; is exactly midnight of the day in question (timezone unclear).
;;; The cdr is a list of ics-objects which start on that day
;;; (endtime might be in another day)
(define *group-evs*
 (group-by event->time *sevs*))

(define *colors*
  '((red #xFF 0 0)
    (green 0 #xFF 0)
    (blue 0 0 #xFF)
    (purple #xFF 0 #xFF)
    (orange #xFF #xFF 0)))

(define (get-rand-color)
  (cdr (list-ref *colors* (random (length *colors*)))))


;;; These should be broken out into an HTML module

(define (date->decimal-hour date)
  "Coverts an SRFI-19 date objects hour and minute field
to a number between 0 and 24"
  (+ (date-hour date)
     (/ (date-minute date)
        60)))

(define (time->decimal-hour time)
  (/ (time-second time)
     3600))

(define (vevent->time field vev)
  (-> vev
      ((extract field))
      (string->date "~Y~m~dT~H~M~S")
      date->time-utc))

(define (vev->sxml vev)
  (let* ((color (get-rand-color))
         (start-time (vevent->time "DTSTART" vev))
         (end-time (vevent->time "DTEND" vev))
         (duration (time-difference end-time start-time))
         (style
             (string-append
              (format #f "top: calc(100%/24 * ~a);"
                      (remainder (floor (time->decimal-hour start-time))
                                 24))
              (format #f "height: calc(100%/24 * ~a);"
                      (floor (time->decimal-hour duration)))
              (apply format #f "border-color: rgba(~a,~a,~a,1);" color)
              (apply format #f "background-color: rgba(~a,~a,~a,0.5);" color))))
    `(div (@ (class "event")
             (style ,style))
          ,((extract "SUMMARY") vev))))



;;; An event group is a list where the car is a time object,
;;; and the cdr is a list of VEVENT's
(define (event-group->sxml evgrp)
  (let ((date (drop-time (time-utc->date (car evgrp)))))
    `(div (@ (class "day"))
          (div (@ (class "meta"))
               (span (@ (class "dayname"))
                     ,(date->string date "~a"))
               (span (@ (class "daydate"))
                     ,(date->string date "~1")))
          (div (@ (class "events"))
               ,@ (map vev->sxml (cdr evgrp))))))


;;; Takesr a list of event groupsr
(define (get-sxml-doc evgrps)
 `(html (head
         (title "Calendar")
         (meta (@ (charset "utf-8")))
         (link (@ (type "text/css")
                  (rel "stylesheet")
                  (href "file/style.css"))))
        (body (div (@ (class "calendar"))
                   ,@ (map (lambda (time)
                             `(div (@ (id ,(string-append "clock-" time))
                                      (class "clock"))
                                   ,(string-append time ":00")))
                           (map number->string (iota 12 0 2)))
                      (div (@ (class "days"))
                           ,@ (map event-group->sxml evgrps))))))



