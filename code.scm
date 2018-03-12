(define-module (code)
  #:use-module (ics)
  #:use-module (ics type object)
  #:use-module (ics type property property)

  #:use-module (srfi srfi-1)             ; List library.
  #:use-module (srfi srfi-8)             ; receive
  #:use-module (srfi srfi-19)            ; Time/Date library.
  #:use-module (srfi srfi-26)            ; Specializing parameters

  ;; These are from my guile-libs
  #:use-module (macros arrow)
  #:use-module (output line)

  #:use-module (oop goops)

  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 curried-definitions)

  #:use-module (util)
  #:use-module (format)
  #:use-module (obj)
  #:use-module (time)

  #:export (get-rand-color 
            vev->sxml event-group->sxml get-sxml-doc
            *sorted-groups*))

;;; Possibly break these out into a file tree module

;;; TODO find better way to check if file is hidden
(define (hidden? filename)
  (string=? "." (string-take filename 1)))

(define (get-cal-name calendar-path)
  (if (not (null? (scandir calendar-path (cut string=? "displayname" <>))))
      (call-with-input-file (path-join* calendar-path "displayname") read-line)
      (basename calendar-path)))

(define (get-files-in-dir path ext)
  "Returns a list of all direct children of <path> which have
the file extension <ext>"
  (map (compose (cut path-join* path <>)
                car)
       (filter (lambda (node)
                 (apply (lambda (name flags . children)
                          (string=? ext (file-extension name)))
                        node))
               (cddr (file-system-tree path)))))

;;; Currently calendars can only be auto detected, and all
;;; have to be direct childs of a common ancestor. Changing
;;; this shouldn't be to hard.

;;; The path of the common ancestor
(define *cal-root*
  (path-join* (getenv "HOME")
              ".calendars"))

;;; List of paths to all specific calendars, it should be
;;; possibly to add extra calendars to this.
(define *cal-paths*
  (map (cut path-join* *cal-root* <>)
       (scandir *cal-root* (negate hidden?))))

;;; Names of all calendars,
;;; this is explicitly here to be able to set colors later
(define *calendars*
  (map get-cal-name *cal-paths*))

;;; list of all ics-objects in filename
;;; parsed as if each file only had one VEVENT
;;; Some aux data is also stored in the object 
(define *ics-objs*
  (append-map (lambda (calendar-path)
         (let* ((files (get-files-in-dir calendar-path "ics"))
                (name (get-cal-name calendar-path)))
           (map (lambda (filename)
                  (-> filename
                      open-input-file
                      ics->scm
                      car
                      ics-object-components
                      car
                      (change-class <ics-path-object>)
                      (slot-set-ret! 'path filename)
                      (slot-set-ret! 'calendar name)))
                files)))
       *cal-paths*))

;;; This whole thing with summary filters should probably be
;;; replaced with filters that can work on any field.
;;; They should also be set in some form of config file
;;; instead of in the program source

;;; Different summary filters
(define (no-filter _) #t)
(define ((contains-filter string) summary)
  (string-contains string summary))

;;; Choose one summary filter
(define summary-filter no-filter)
;; (define summary-filter (contains-filter "THEN18"))

;;; Apply it
(define *filtered-events*
  (filter-on-property "SUMMARY"
                      summary-filter
                      *ics-objs*))

;;; Sorted events
(define *sevs* (sort* *filtered-events* time<? event-time)) 


;;; Each element is a day, the car is a time-utc object, which
;;; is exactly midnight of the day in question (timezone unclear).
;;; The cdr is a list of ics-objects which start on that day
;;; (endtime might be in another day)

(define *group-evs*
 (group-by event-date *sevs*))

(define *sorted-groups*
  (sort* *group-evs* time<? (compose date->time-utc car)))

;;; Takes a list on the form
;;; ((date calendar-obj ...) ...)
;;; And makes each sublist have better laid out elements.
;;; It's not perfect if there are many elements that overlap
;;; In different ways. But it works perfectly for a block
;;; schedule!
(define (fix-event-widths evs)
  (define (fix-within-day day-evs)
    (if (null? day-evs)
        #f
        (let ((ev-list day-evs))
          (receive (overlapping rest)
              (take-and-drop-while
               (lambda (next)
                 (time<? (date->time-utc (start next))
                         (date->time-utc (end (car ev-list)))))
               ev-list)
            (for-each set-x! overlapping (iota 10))
            (for-each (cut set-width! <> (/ (length overlapping)))
                      overlapping)
            (fix-within-day rest)))))
  (for-each (compose fix-within-day cdr)
            evs))

(fix-event-widths *group-evs*)

(define *colors*
  '((red #xFF 0 0)
    (green 0 #xFF 0)
    (blue 0 0 #xFF)
    (purple #xFF 0 #xFF)
    (orange #xFF #xFF 0)))

(define (get-rand-color)
  (cdr (list-ref *colors* (random (length *colors*)))))

;;; These should be broken out into an HTML module

(define *calendar-colors*
  (map cons *calendars*
       (map car *colors*)))

(define (color-by-calendar event)
  (assoc-ref *calendar-colors*
             (containing-calendar event)))

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
    `(div (@ (class ,(string-join `("event" ,(symbol->string color)) " " 'infix))
             (style ,style))
          ,(summary-proc vev))))


;;; An event group is a list where the car is a time object,
;;; and the cdr is a list of VEVENT's
(define (event-group->sxml evgrp)
  (let ((date (car evgrp)))
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
                  (href "file/style.css")))
         (script (@ (src "file/jquery-3.3.1.min.js")) "")
         (script (@ (src "file/script.js")) ""))
        (body (div (@ (class "calendar"))
                   ,@ (map (lambda (time)
                             `(div (@ (id ,(string-append "clock-" time))
                                      (class "clock"))
                                   ,(string-append time ":00")))
                           (map number->string (iota 12 0 2)))
                      (div (@ (class "days"))
                           ,@ (map event-group->sxml evgrps))))))



