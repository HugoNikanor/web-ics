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
  ;; #:use-module (css)
  #:use-module (output line)

  #:use-module (oop goops)

  #:use-module (ice-9 ftw)
  ;; #:use-module (ice-9 format)
  ;; (ice-9 rdelim)

  #:use-module (util)
  #:use-module (format)
  #:use-module (obj)
  #:use-module (time)

  #:export (get-rand-color 
            vev->sxml event-group->sxml get-sxml-doc
            *group-evs*))

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


(define *cal-path*
  (string-append
   (getenv "HOME")
   ;; "/.calendars/b85ba2e9-18aa-4451-91bb-b52da930e977/"
   "/.calendars/D2.b/"
   ))

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
                        (=  16 (string-length time)))
                      *ics-objs*))

;;; Sorted events
(define *sevs* (sort* *limited-events* time<? event-time)) 


;;; Each element is a day, the car is a time-utc object, which
;;; is exactly midnight of the day in question (timezone unclear).
;;; The cdr is a list of ics-objects which start on that day
;;; (endtime might be in another day)

(define *group-evs*
 (group-by event-date *sevs*))

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
  "This should only be used on time intervals,
never on absolute times. For that see date->decimal-hour"
  (exact->inexact (/ (time-second time)
                     3600)))


(define (date->decimal-hour date)
  (exact->inexact
   (+ (date-hour date)
      (/ (date-minute date)
         60))))

(define (vev->sxml vev)
  (let* ((color (get-rand-color))
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
              (apply format #f "border-color: rgba(~a,~a,~a,1);" color)
              (apply format #f "background-color: rgba(~a,~a,~a,0.5);" color))))
    `(div (@ (class "event")
             (style ,style))
          ,(strip-summary-liu ((extract "SUMMARY") vev))
          ;; ,((extract "SUMMARY") vev)
          )))



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
                  (href "file/style.css"))))
        (body (div (@ (class "calendar"))
                   ,@ (map (lambda (time)
                             `(div (@ (id ,(string-append "clock-" time))
                                      (class "clock"))
                                   ,(string-append time ":00")))
                           (map number->string (iota 12 0 2)))
                      (div (@ (class "days"))
                           ,@ (map event-group->sxml evgrps))))))



