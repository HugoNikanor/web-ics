;;; SUMMARY är tydligen kort-beskrivning, och inte förklaring

(map (compose ;; ics-property-value
      (cut ics-object-property-ref <> "SEQUENCE")
      cadr)
     fname-obj-list)

(ics-object-property-ref o "UID")
(ics-property-parameter-ref o "UID")

(filter-on-property identity "SEQUENCE" fname-obj-list)


(map ics-property-name
     (ics-object-properties (cadr (car fname-obj-list))))
("SEQUENCE" "UID" "DTSTAMP" "DTEND" "DTSTART" "SUMMARY")


(map-on-property identity "DTSTART" fname-obj-list)

(catch 'misc-error
  (lambda ()
    (string->date "20180220" "~Y~m~dT~H~M~S"))
  list)

(cut string->date <> "~Y~m~dT~H~M~S")
(sort* fname-obj-list time<?
       (lambda (item)
         (string->date
          (ics-property-value
           (ics-object-property-ref (cadr item)
                                    "DTSTART"))
          "~Y~m~dT~H~M~S"))) 

(filter-on-property
 "SUMMARY"
 (cut string-contains <> "D")
 ics-objs) 



(string-contains "HelDlo" "D") 

(define-class <ics-path-object> (<ics-object>)
  path)

(fileskip0)

(sort* ics-objs )

;;; This is the *WORST* way to check for events which aren't all day
(define limited-events
 (filter-on-property "DTSTART"
                     (lambda (time)
                       (=  15 (string-length time)))
                     ics-objs))

(sort* limited-events
       (lambda (item)
         (string->date (extract "DTSTART")
                       "~Y~m~dT~H~M~S"))
       time<?)

((extract "DTSTART") (car limited-events))
;; "20180214T170000"

(string->date ((extract "DTSTART") (car limited-events))
              "~Y~m~dT~H~M~S")
;; #<date nanosecond: 0 second: 0 minute: 0 hour: 17 day: 14 month: 2 year: 2018 zone-offset: 3600>


(sort* limited-events time<? event-time)


(define tt (event-time (car limited-events)))

tt #<date nanosecond: 0 second: 0 minute: 0 hour: 17 day: 14 month: 2 year: 2018 zone-offset: 3600>
(current-time) #<time type: time-utc nanosecond: 887337000 second: 1519408137>
(date->time-utc tt) #<time type: time-utc nanosecond: 0 second: 1518624000>

;;; Dates are apparently not times...


tt #<date nanosecond: 0 second: 0 minute: 0 hour: 17 day: 14 month: 2 year: 2018 zone-offset: 3600>
(event-time (car limited-events)) #<time type: time-utc nanosecond: 0 second: 1518624000>





(time-type tt) ERRORError: retort-syntax

;;; Sorted events
(define *sevs* (sort* limited-events time<? event-time)) 

(define-class <spacer> ()
  (char #:init-keyword #:c
        #:init-value #\-
        #:getter c)
  (length #:init-keyword #:l
          #:init-value 40
          #:getter l))



(define-method (describe (obj <spacer>))
  (display (make-string (l obj)
                        (c obj)))
  (newline))

(describe (make <spacer>))
(describe (make <spacer> #:c #\& #:l 20))

(int)

(flatten '(1 2 3 4)) (4 3 2 1)
 
(flatten '(1 ((a b) 3) 4)) #; (4 3 b a 1)
(flatten '(1 ((a b) 3) 4))  (1 a b 3 4)

(inter)

(intersperce (iota 10) 'a) (a 0 a 1 a 2 a 3 a 4 a 5 a 6 a 7 a 8 a 9)

nil
 ERRORError: retort-syntax

(define-once nil '()) #<unspecified>

(define-macro )

()









