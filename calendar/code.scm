(define-module (calendar code)
  #:use-module (ics)
  #:use-module (ics type object)
  #:use-module (ics type property property)

  #:use-module (srfi srfi-1)             ; List library.
  #:use-module (srfi srfi-19)            ; Time/Date library.
  #:use-module (srfi srfi-26)            ; Specializing parameters

  ;; These are from my guile-libs
  #:use-module (macros arrow)
  #:use-module (output line)

  #:use-module (oop goops)

  #:use-module (ice-9 ftw)
  #:use-module (ice-9 curried-definitions)

  #:use-module (calendar util)
  #:use-module (calendar format)
  #:use-module (calendar obj)
  #:use-module (calendar time)

  #:use-module (calendar fs)
  #:use-module (calendar display)
  #:use-module (calendar gui html)

  #:export (get-rand-color 
            vev->sxml event-group->sxml
            load-calendars))

(define (load-calendars calendar-root)
  "Load all calendars into a list."
  (let* ((cal-paths (get-cal-paths calendar-root))
         (cal-names (-> cal-paths get-calendar-names))
         (evgrps (-> cal-paths
                     get-ics-objects           ; Slow
                     sort-events
                     group-events
                     sort-groups)))
    (fix-event-widths evgrps)
    (get-sxml-doc evgrps cal-names)))

;;; List of paths to all specific calendars, it should be
;;; possibly to add extra calendars to this.
(define (get-cal-paths cal-root)
  (map (cut path-join* cal-root <>)
       (scandir cal-root (negate hidden?))))

;;; Names of all calendars,
;;; this is explicitly here to be able to set colors later
(define (get-calendar-names cal-paths)
  (map get-cal-name cal-paths))

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
                     (lambda (err-symb fmt-port fmt-str fmt-args . args)
                       (format (current-error-port)
                               "~a: ~? in ~%~a~%"
                               err-symb
                               fmt-str fmt-args
                               filename)
                       #;
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

;;; Rewrite rules for the summaries
;;; TODO should be placed into some form of config file
(define (summary-proc vev)
  (let ((summary ((extract "SUMMARY") vev)))
    (cond ((member (containing-calendar vev)
                   '("D1.b" "D2.b" "D3.b"))
           (strip-summary-liu summary))
          (else summary))))

