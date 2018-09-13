(define-module (code)
  #:use-module (ics)
  #:use-module (ics type object)
  #:use-module (ics type property property)

  #:use-module (srfi srfi-1)             ; List library.
  #:use-module (srfi srfi-19)            ; Time/Date library.
  #:use-module (srfi srfi-26)            ; Specializing parameters
  #:use-module (srfi srfi-41)            ; Streams

  ;; These are from my guile-libs
  #:use-module (macros arrow)
  #:use-module (output line)

  #:use-module (oop goops)

  #:use-module (ice-9 ftw)
  #:use-module (ice-9 curried-definitions)

  #:use-module (util)
  #:use-module (format)
  #:use-module (obj)
  #:use-module (time)

  #:use-module (fs)
  #:use-module (display)

  #:export (get-rand-color 
            vev->sxml event-group->sxml get-sxml-doc
            do-stuff))

;;; The path of the common ancestor
(define *cal-root*
  (path-join* (getenv "HOME")
              ".calendars"))

;;; List of paths to all specific calendars, it should be
;;; possibly to add extra calendars to this.
(define (get-cal-paths cal-root)
  (map (cut path-join* cal-root <>)
       (scandir cal-root (negate hidden?))))

;;; Names of all calendars,
;;; this is explicitly here to be able to set colors later
(define (get-calendar-names cal-paths)
  (map get-cal-name cal-paths))

(define (rev-filter lst filt)
  "Like filter, but takes arguments in reverse order"
  (filter filt lst))

;;; list of all ics-objects in filename
;;; parsed as if each file only had one VEVENT
;;; Some aux data is also stored in the object 

;;; ics->scm returns a list of the VCALENDAR objects in the file.
;;; In my case there is never more than 1
(define (get-ics-objects calendar-paths)
  (append-map (lambda (calendar-path)
         (let* ((files (get-files-in-dir calendar-path "ics"))
                (name (get-cal-name calendar-path)))
           (filter
            identity
            (map (lambda (filename)
                   (catch 'misc-error
                     (lambda ()
                       (-> filename
                           open-input-file
                           ics->scm
                           car
                           ics-object-components
                           car
                           (change-class <ics-path-object>)
                           (slot-set-ret! 'path filename)
                           (slot-set-ret! 'calendar name)))
                     (lambda (err . args)
                       (format (current-error-port) "~a: ~s\n" err args)
                       #f)))
                 files))))
       calendar-paths))

;;; Apply it
#; 
(define *filtered-events*
  (filter-on-property "SUMMARY"
                      summary-filter
                      *ics-objs*))

;;; Sorted events
(define (sort-events events)
  "Sortes events by start time"
  (sort* events time<? event-time)) 


;;; Each element is a day, the car is a time-utc object, which
;;; is exactly midnight of the day in question (timezone unclear).
;;; The cdr is a list of ics-objects which start on that day
;;; (endtime might be in another day)

;;; TODO I currently only throw away the timezone information.
;;; Events are still laid out correctly, but events that start
;;; /near/ midnight might be placed in the wrong day.
;;; 
;;; /Near/ defined as events closer to midnight than their zone
;;; offset.
(define (group-events events)
  "Returns groups of elements"
 (group-by (compose drop-zone-offset event-date) events))

(define (sort-groups groups)
  "Return groups of elementns, sorts by start date"
  (sort* groups time<? (compose date->time-utc car)))

#; (fix-event-widths *group-evs*)

(define *colors*
  '((red #xFF 0 0)
    (green 0 #xFF 0)
    (blue 0 0 #xFF)
    (purple #xFF 0 #xFF)
    (orange #xFF #xFF 0)))

(define (get-rand-color)
  (cdr (list-ref *colors* (random (length *colors*)))))

;;; These should be broken out into an HTML module

(define-stream (color-stream)
  (stream-cons (list (random #x100)
                     (random #x100)
                     (random #x100))
               (color-stream)))

(define (get-calendar-colors calendar-names)
  "Returns a list of pairs between names and colors.
cdr is either a symbol which is the name of the color,
or a list of RGB."
  (stream->list (stream-map cons (list->stream calendar-names)
                            (color-stream)))
  #;
  (map cons calendar-names
       (map car *colors*)))

(define (color-by-calendar event)
  (-> *cal-root*
      get-cal-paths
      get-calendar-names
      get-calendar-colors
      (assoc-ref (containing-calendar event))))

;;; Rewrite rules for the summaries
;;; TODO should be placed into some form of config file
(define (summary-proc vev)
  (let ((summary ((extract "SUMMARY") vev)))
    (cond ((member (containing-calendar vev)
                   '("D1.b" "D2.b"))
           (strip-summary-liu summary))
          (else summary))))

(define (vev->sxml vev)
  (let* ((color (color-by-calendar vev))
         (start-time (vevent->time "DTSTART" vev))
         (start-date (time-utc->date start-time))

         (end-time (vevent->time "DTEND" vev))
         (end-date (time-utc->date end-time))

         (duration (time-difference end-time start-time))
         (style
             (string-append
              (format #f "top: calc(100%/24 * ~a);"
                      (date->decimal-hour start-date))
              (format #f "height: calc(100%/24 * ~a);"
                      (time->decimal-hour duration)
                      ;; (date->decimal-hour duration)
                      )
              (format #f "width: calc(100% * (~a));" (get-width vev))
              (format #f "left: calc(100% * (~a) * (~a));"
                      (get-width vev)
                      (get-x vev))
              
             )))
    `(div (@ (class ,(string-join `("event" ,(css-ify (containing-calendar vev))) " " 'infix))
             (style ,style))
          ,(summary-proc vev))))


;;; An event group is a list where the car is a time object,
;;; and the cdr is a list of VEVENT's
(define (event-group->sxml evgrp)
  (let* ((date (car evgrp))
         (datestr (date->string date "~1")))
    `(div (@ (class "day")
             (id ,datestr))
          (div (@ (class "meta"))
               (span (@ (class "dayname"))
                     ,(date->string date "~a"))
               (span (@ (class "daydate"))
                     ,datestr))
          (div (@ (class "events"))
               ,@ (map vev->sxml (cdr evgrp))))))


;;; Takesr a list of event groupsr
(define (get-sxml-doc evgrps calendar-names)
 `(html (head
         (title "Calendar")
         (meta (@ (charset "utf-8")))
         (link (@ (type "text/css")
                  (rel "stylesheet")
                  (href "file/style.css")))
         (script (@ (src "file/jquery-3.3.1.min.js")) "")
         (script (@ (src "file/script.js")) "")
         (style
             ,@(map (lambda (cal color)
                      (format #f ".~a { background-color: ~a; }"
                              (css-ify cal)
                              (if (symbol? (cdr color))
                                  (cdr color)
                                  (apply format #f "rgb(~a,~a,~a,0.9)"
                                         (cdr color)))))
                    calendar-names
                    (get-calendar-colors calendar-names))))
        (body (span (@ (id "gen-time"))
                    ,(date->string (current-date) "Last Updated: ~c"))
              (button (@ (id "goto-today")
                         (onclick "gotoToday()"))
                      "Goto Today")
              (div (@ (class "calendar"))
                   ,@ (map (lambda (time)
                             `(div (@ (id ,(string-append "clock-" time))
                                      (class "clock"))
                                   ,(string-append time ":00")))
                           (map number->string (iota 12 0 2)))
                   (div (@ (class "days"))
                        ,@ (map event-group->sxml evgrps))))))




(define (do-stuff)
  (let* ((cal-paths (get-cal-paths *cal-root*))
         (cal-names (-> cal-paths get-calendar-names))
         (evgrps (-> cal-paths
                     get-ics-objects           ; Slow
                     sort-events
                     group-events
                     sort-groups)))
    (fix-event-widths evgrps)
    (get-sxml-doc evgrps cal-names)))
