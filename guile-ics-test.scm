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

(define al
  '((a (1 2))
    (b (a b))))

(assoc 'a al) #; (a (1 2))

(acons 'a 10 al) ((a . 10) (a (1 2)) (b (a b)))

(define l '(a b)) #<unspecified>

(append! l '(a b)) (a b 1 2 a b)


(group-by even? (iota 10))
()
((#f . 9) (#t . 8) (#f . 7) (#t . 6) (#f . 5) (#t . 4) (#f . 3) (#t . 2) (#f . 1) (#t . 0))

(call-with-values (lambda () (values 1 2 3))
  list)


(eqv? #t #t) #t
(eqv? "a" "a") #t
(eqv? '(a b) '(a b)) #t




(define lst (iota 10))
(define keys '())
(define func even?)


(fold (lambda (item alist)
        (acons (car (append! keys (list (func item))))
               item
               alist))
      '()
      lst)


(append! keys (list (even? 0))) (#t)

;;; ----------------------------------------



 (#f #t #f #t #f #t #f #t #f #t)
((#f . 9) (#t . 8) (#f . 7) (#t . 6) (#f . 5) (#t . 4) (#f . 3) (#t . 2) (#f . 1) (#t . 0))



(define (group-by func lst)
  (let* ((keys '())
         (alist (fold (lambda (item alist)
                        (acons (car (append! keys (list (func item))))
                               item
                               alist))
                      '()
                      lst)))
    (values keys alist)
    ))
    ;; (map (lambda (key)
    ;;        (filter (lambda (pair)
    ;;                  ;; This should probably by <equal?>
    ;;                  (eqv? key (car pair)))
    ;;                alist))
    ;;      keys)))



(group-by even? (iota 10))
 ((#f 1 3 5 7 9) (#t 0 2 4 6 8))

(group-by (cut remainder <> 3)
          (iota 10))
 ((0 0 3 6 9) (2 2 5 8) (1 1 4 7))


(assv-set! l 'a 10) ((a . 10))
l ()
(assv-ref '((a 10)) 'a) (10)

(define)
(assv-set! l 'a (cons 20 (assv-ref l 'a)))



(a . 10)


(event-time (car *sevs*))

(for-each
 displayln
 (sort* (map car *group-evs*)
        time<? date->time-utc))

#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 18 month: 1 year: 2017 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 5 month: 1 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 9 month: 1 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 11 month: 1 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 15 month: 1 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 17 month: 1 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 18 month: 1 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 20 month: 1 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 23 month: 1 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 24 month: 1 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 25 month: 1 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 31 month: 1 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 2 month: 2 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 7 month: 2 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 8 month: 2 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 9 month: 2 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 11 month: 2 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 12 month: 2 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 13 month: 2 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 14 month: 2 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 15 month: 2 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 16 month: 2 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 17 month: 2 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 19 month: 2 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 22 month: 2 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 23 month: 2 year: 2018 zone-offset: 3600>
#<date nanosecond: 0 second: 0 minute: 0 hour: 0 day: 16 month: 3 year: 2018 zone-offset: 3600>

(for-each displayln (map (extract "DTSTART") *ics-objs*))

20180214T170000
20180212T130000
20180208T220000
20180118
20180316T100000
20180131
20180223T103000
20180222T130000
20180216T080000
20180212T090000
20180118T190000
20180109T140000
20180125T130000
20180116T140000Z
20180323
20180124T151500
20180215T170000
20180223T130000
20180105T170000Z
20180120T180000
20180202T130000
20180209T180000
20180219T080000Z
20180223T070000Z
20180219T160000
20180126
20180214T150000
20180115T180000
20180131T150000
20180116T083000Z
20180111T140000
20170118T000000
20180314
20180219T180000
20180129
20180217T180000
20180213T190000
20180207T140000
20180222T180000
20180123T090000
20180118T190000
20180207T092000
20180220
20180105T173000
20180131
20180117T170000
20180211T150000
20180215

(for-each displayln (map (extract "DTSTART") *limited-events*))

20180214T170000
20180212T130000
20180208T220000
20180316T100000
20180223T103000
20180222T130000
20180216T080000
20180212T090000
20180118T190000
20180109T140000
20180125T130000
20180124T151500
20180215T170000
20180223T130000
20180120T180000
20180202T130000
20180209T180000
20180219T160000
20180214T150000
20180115T180000
20180131T150000
20180111T140000
20170118T000000
20180219T180000
20180217T180000
20180213T190000
20180207T140000
20180222T180000
20180123T090000
20180118T190000
20180207T092000
20180105T173000
20180117T170000
20180211T150000

(for-each displayln (map (extract "DTSTART") *sevs*))

20170118T000000
20180105T173000
20180109T140000
20180111T140000
20180115T180000
20180117T170000
20180118T190000
20180118T190000
20180120T180000
20180123T090000
20180124T151500
20180125T130000
20180131T150000
20180202T130000
20180207T092000
20180207T140000
20180208T220000
20180209T180000
20180211T150000
20180212T090000
20180212T130000
20180213T190000
20180214T150000
20180214T170000
20180215T170000
20180216T080000
20180217T180000
20180219T160000
20180219T180000
20180222T130000
20180222T180000
20180223T103000
20180223T130000
20180316T100000

(for-each displayln (map (lambda (item)
                           (-> item
                               ((extract "DTSTART"))
                               (string->date "~Y~m~dT~H~M~S")
                               drop-time))
                         *sevs*))


(define (f ev)
  (-> ev
      ((extract "DTSTART"))
      (string->date "~Y~m~dT~H~M~S")
      drop-time
      date->time-utc
      ))
(f (list-ref *sevs* 6)) #<time type: time-utc nanosecond: 0 second: 1516230000>
(f (list-ref *sevs* 7)) #<time type: time-utc nanosecond: 0 second: 1516230000>

(time=? (f (list-ref *sevs* 6))
        (f (list-ref *sevs* 7))) #t

(equal? (f (list-ref *sevs* 6))
        (f (list-ref *sevs* 7))) #t

(define *group-evs* (group-by f *sevs*))

(for-each displayln 
          (map cons
               (map f *sevs*)
               *sevs*))

 0 (#<time type: time-utc nanosecond: 0 second: 1484694000> . #<ics-object VEVENT 55f75d551e10>)
 1 (#<time type: time-utc nanosecond: 0 second: 1515106800> . #<ics-object VEVENT 55f75d8be6f0>)
 2 (#<time type: time-utc nanosecond: 0 second: 1515452400> . #<ics-object VEVENT 55f75d836d80>)
 3 (#<time type: time-utc nanosecond: 0 second: 1515625200> . #<ics-object VEVENT 55f75dcdf450>)
 4 (#<time type: time-utc nanosecond: 0 second: 1515970800> . #<ics-object VEVENT 55f75d790540>)
 5 (#<time type: time-utc nanosecond: 0 second: 1516143600> . #<ics-object VEVENT 55f75dba13c0>)
 6 (#<time type: time-utc nanosecond: 0 second: 1516230000> . #<ics-object VEVENT 55f75d739fc0>)
 7 (#<time type: time-utc nanosecond: 0 second: 1516230000> . #<ics-object VEVENT 55f75d75cc60>)
 8 (#<time type: time-utc nanosecond: 0 second: 1516402800> . #<ics-object VEVENT 55f75dabae70>)
 9 (#<time type: time-utc nanosecond: 0 second: 1516662000> . #<ics-object VEVENT 55f75dd10f60>)
10 (#<time type: time-utc nanosecond: 0 second: 1516748400> . #<ics-object VEVENT 55f75d787ae0>)
11 (#<time type: time-utc nanosecond: 0 second: 1516834800> . #<ics-object VEVENT 55f75d551990>)
12 (#<time type: time-utc nanosecond: 0 second: 1517353200> . #<ics-object VEVENT 55f75d8369c0>)
13 (#<time type: time-utc nanosecond: 0 second: 1517526000> . #<ics-object VEVENT 55f75d7f87e0>)
14 (#<time type: time-utc nanosecond: 0 second: 1517958000> . #<ics-object VEVENT 55f75dd217e0>)
15 (#<time type: time-utc nanosecond: 0 second: 1517958000> . #<ics-object VEVENT 55f75cf64660>)
16 (#<time type: time-utc nanosecond: 0 second: 1518044400> . #<ics-object VEVENT 55f75dbb6810>)
17 (#<time type: time-utc nanosecond: 0 second: 1518130800> . #<ics-object VEVENT 55f75d882f30>)
18 (#<time type: time-utc nanosecond: 0 second: 1518303600> . #<ics-object VEVENT 55f75d30cf00>)
19 (#<time type: time-utc nanosecond: 0 second: 1518390000> . #<ics-object VEVENT 55f75d9bfcc0>)
20 (#<time type: time-utc nanosecond: 0 second: 1518390000> . #<ics-object VEVENT 55f75dc0a960>)
21 (#<time type: time-utc nanosecond: 0 second: 1518476400> . #<ics-object VEVENT 55f75d4ba300>)
22 (#<time type: time-utc nanosecond: 0 second: 1518562800> . #<ics-object VEVENT 55f75d8a3600>)
23 (#<time type: time-utc nanosecond: 0 second: 1518562800> . #<ics-object VEVENT 55f75d279900>)
24 (#<time type: time-utc nanosecond: 0 second: 1518649200> . #<ics-object VEVENT 55f75d836c00>)
25 (#<time type: time-utc nanosecond: 0 second: 1518735600> . #<ics-object VEVENT 55f75d2b1f90>)
26 (#<time type: time-utc nanosecond: 0 second: 1518822000> . #<ics-object VEVENT 55f75dc8e720>)
27 (#<time type: time-utc nanosecond: 0 second: 1518994800> . #<ics-object VEVENT 55f75d8820c0>)
28 (#<time type: time-utc nanosecond: 0 second: 1518994800> . #<ics-object VEVENT 55f75dba1150>)
29 (#<time type: time-utc nanosecond: 0 second: 1519254000> . #<ics-object VEVENT 55f75da4a1e0>)
30 (#<time type: time-utc nanosecond: 0 second: 1519254000> . #<ics-object VEVENT 55f75dbb7ea0>)
31 (#<time type: time-utc nanosecond: 0 second: 1519340400> . #<ics-object VEVENT 55f75d30c7b0>)
32 (#<time type: time-utc nanosecond: 0 second: 1519340400> . #<ics-object VEVENT 55f75d551480>)
33 (#<time type: time-utc nanosecond: 0 second: 1521154800> . #<ics-object VEVENT 55f75dcdf3c0>)

(call-with-values
    (lambda ()
      (let ((func event->time)
            (list *sevs*))
        (fold-multiple
         (lambda (item keys alist)
           (let ((key (func item)))
             (values (cons key keys)
                     (acons key item alist))))
         list
         '() '())))
  (lambda (keys alist)
    (displayln "KEYS")
    (for-each displayln keys)
    (displayln "VALUES")
    (for-each displayln alist)))

KEYS
#<time type: time-utc nanosecond: 0 second: 1484694000>
#<time type: time-utc nanosecond: 0 second: 1515106800>
#<time type: time-utc nanosecond: 0 second: 1515452400>
#<time type: time-utc nanosecond: 0 second: 1515625200>
#<time type: time-utc nanosecond: 0 second: 1515970800>
#<time type: time-utc nanosecond: 0 second: 1516143600>
#<time type: time-utc nanosecond: 0 second: 1516230000>
#<time type: time-utc nanosecond: 0 second: 1516230000>
#<time type: time-utc nanosecond: 0 second: 1516402800>
#<time type: time-utc nanosecond: 0 second: 1516662000>
#<time type: time-utc nanosecond: 0 second: 1516748400>
#<time type: time-utc nanosecond: 0 second: 1516834800>
#<time type: time-utc nanosecond: 0 second: 1517353200>
#<time type: time-utc nanosecond: 0 second: 1517526000>
#<time type: time-utc nanosecond: 0 second: 1517958000>
#<time type: time-utc nanosecond: 0 second: 1517958000>
#<time type: time-utc nanosecond: 0 second: 1518044400>
#<time type: time-utc nanosecond: 0 second: 1518130800>
#<time type: time-utc nanosecond: 0 second: 1518303600>
#<time type: time-utc nanosecond: 0 second: 1518390000>
#<time type: time-utc nanosecond: 0 second: 1518390000>
#<time type: time-utc nanosecond: 0 second: 1518476400>
#<time type: time-utc nanosecond: 0 second: 1518562800>
#<time type: time-utc nanosecond: 0 second: 1518562800>
#<time type: time-utc nanosecond: 0 second: 1518649200>
#<time type: time-utc nanosecond: 0 second: 1518735600>
#<time type: time-utc nanosecond: 0 second: 1518822000>
#<time type: time-utc nanosecond: 0 second: 1518994800>
#<time type: time-utc nanosecond: 0 second: 1518994800>
#<time type: time-utc nanosecond: 0 second: 1519254000>
#<time type: time-utc nanosecond: 0 second: 1519254000>
#<time type: time-utc nanosecond: 0 second: 1519340400>
#<time type: time-utc nanosecond: 0 second: 1519340400>
#<time type: time-utc nanosecond: 0 second: 1521154800>

VALUES
(#<time type: time-utc nanosecond: 0 second: 1484694000> . #<ics-object VEVENT 55f75d551e10>)
(#<time type: time-utc nanosecond: 0 second: 1515106800> . #<ics-object VEVENT 55f75d8be6f0>)
(#<time type: time-utc nanosecond: 0 second: 1515452400> . #<ics-object VEVENT 55f75d836d80>)
(#<time type: time-utc nanosecond: 0 second: 1515625200> . #<ics-object VEVENT 55f75dcdf450>)
(#<time type: time-utc nanosecond: 0 second: 1515970800> . #<ics-object VEVENT 55f75d790540>)
(#<time type: time-utc nanosecond: 0 second: 1516143600> . #<ics-object VEVENT 55f75dba13c0>)
(#<time type: time-utc nanosecond: 0 second: 1516230000> . #<ics-object VEVENT 55f75d739fc0>)
(#<time type: time-utc nanosecond: 0 second: 1516230000> . #<ics-object VEVENT 55f75d75cc60>)
(#<time type: time-utc nanosecond: 0 second: 1516402800> . #<ics-object VEVENT 55f75dabae70>)
(#<time type: time-utc nanosecond: 0 second: 1516662000> . #<ics-object VEVENT 55f75dd10f60>)
(#<time type: time-utc nanosecond: 0 second: 1516748400> . #<ics-object VEVENT 55f75d787ae0>)
(#<time type: time-utc nanosecond: 0 second: 1516834800> . #<ics-object VEVENT 55f75d551990>)
(#<time type: time-utc nanosecond: 0 second: 1517353200> . #<ics-object VEVENT 55f75d8369c0>)
(#<time type: time-utc nanosecond: 0 second: 1517526000> . #<ics-object VEVENT 55f75d7f87e0>)
(#<time type: time-utc nanosecond: 0 second: 1517958000> . #<ics-object VEVENT 55f75dd217e0>)
(#<time type: time-utc nanosecond: 0 second: 1517958000> . #<ics-object VEVENT 55f75cf64660>)
(#<time type: time-utc nanosecond: 0 second: 1518044400> . #<ics-object VEVENT 55f75dbb6810>)
(#<time type: time-utc nanosecond: 0 second: 1518130800> . #<ics-object VEVENT 55f75d882f30>)
(#<time type: time-utc nanosecond: 0 second: 1518303600> . #<ics-object VEVENT 55f75d30cf00>)
(#<time type: time-utc nanosecond: 0 second: 1518390000> . #<ics-object VEVENT 55f75d9bfcc0>)
(#<time type: time-utc nanosecond: 0 second: 1518390000> . #<ics-object VEVENT 55f75dc0a960>)
(#<time type: time-utc nanosecond: 0 second: 1518476400> . #<ics-object VEVENT 55f75d4ba300>)
(#<time type: time-utc nanosecond: 0 second: 1518562800> . #<ics-object VEVENT 55f75d8a3600>)
(#<time type: time-utc nanosecond: 0 second: 1518562800> . #<ics-object VEVENT 55f75d279900>)
(#<time type: time-utc nanosecond: 0 second: 1518649200> . #<ics-object VEVENT 55f75d836c00>)
(#<time type: time-utc nanosecond: 0 second: 1518735600> . #<ics-object VEVENT 55f75d2b1f90>)
(#<time type: time-utc nanosecond: 0 second: 1518822000> . #<ics-object VEVENT 55f75dc8e720>)
(#<time type: time-utc nanosecond: 0 second: 1518994800> . #<ics-object VEVENT 55f75d8820c0>)
(#<time type: time-utc nanosecond: 0 second: 1518994800> . #<ics-object VEVENT 55f75dba1150>)
(#<time type: time-utc nanosecond: 0 second: 1519254000> . #<ics-object VEVENT 55f75da4a1e0>)
(#<time type: time-utc nanosecond: 0 second: 1519254000> . #<ics-object VEVENT 55f75dbb7ea0>)
(#<time type: time-utc nanosecond: 0 second: 1519340400> . #<ics-object VEVENT 55f75d30c7b0>)
(#<time type: time-utc nanosecond: 0 second: 1519340400> . #<ics-object VEVENT 55f75d551480>)
(#<time type: time-utc nanosecond: 0 second: 1521154800> . #<ics-object VEVENT 55f75dcdf3c0>)

(call-with-values
    (lambda ()
      (let ((func event->time)
            (list *sevs*))
        (fold-multiple
         (lambda (item keys alist)
           (let ((key (func item)))
             (values (cons key keys)
                     (acons key item alist))))
         list
         '() '())))
  (lambda (keys alist)
    (displayln "KEYS")
    (for-each displayln (unique keys))))

KEYS
 0 #<time type: time-utc nanosecond: 0 second: 1521154800>
 1 #<time type: time-utc nanosecond: 0 second: 1519340400>
 2 #<time type: time-utc nanosecond: 0 second: 1519254000>
 3 #<time type: time-utc nanosecond: 0 second: 1518994800>
 4 #<time type: time-utc nanosecond: 0 second: 1518822000>
 5 #<time type: time-utc nanosecond: 0 second: 1518735600>
 6 #<time type: time-utc nanosecond: 0 second: 1518649200>
 7 #<time type: time-utc nanosecond: 0 second: 1518562800>
 8 #<time type: time-utc nanosecond: 0 second: 1518476400>
 9 #<time type: time-utc nanosecond: 0 second: 1518390000>
10 #<time type: time-utc nanosecond: 0 second: 1518303600>
11 #<time type: time-utc nanosecond: 0 second: 1518130800>
12 #<time type: time-utc nanosecond: 0 second: 1518044400>
13 #<time type: time-utc nanosecond: 0 second: 1517958000>
14 #<time type: time-utc nanosecond: 0 second: 1517526000>
15 #<time type: time-utc nanosecond: 0 second: 1517353200>
16 #<time type: time-utc nanosecond: 0 second: 1516834800>
17 #<time type: time-utc nanosecond: 0 second: 1516748400>
18 #<time type: time-utc nanosecond: 0 second: 1516662000>
19 #<time type: time-utc nanosecond: 0 second: 1516402800>
20 #<time type: time-utc nanosecond: 0 second: 1516230000>
21 #<time type: time-utc nanosecond: 0 second: 1516143600>
22 #<time type: time-utc nanosecond: 0 second: 1515970800>
23 #<time type: time-utc nanosecond: 0 second: 1515625200>
24 #<time type: time-utc nanosecond: 0 second: 1515452400>
25 #<time type: time-utc nanosecond: 0 second: 1515106800>
26 #<time type: time-utc nanosecond: 0 second: 1484694000>

(call-with-values
    (lambda ()
      (let ((func event->time)
            (list *sevs*))
        (fold-multiple
         (lambda (item keys alist)
           (let ((key (func item)))
             (values (cons key keys)
                     (acons key item alist))))
         list
         '() '())))
  (lambda (keys alist)
    (list-ref (unique keys) 20)))

#<time type: time-utc nanosecond: 0 second: 1516230000>

(let ((func event->time)
      (list *sevs*))
  (receive (keys alist)
      (fold-multiple
       (lambda (item keys alist)
         (let ((key (func item)))
           (values (cons key keys)
                   (acons key item alist))))
       list
       '() '())

    (map (lambda (key)
           (cons key (map cdr (filter (lambda (item)
                                        (equal? key (car item)))
                                      alist))))
         (unique keys))))

(group-by event->time *sevs*)
((#<time type: time-utc nanosecond: 0 second: 1521154800> #<ics-object VEVENT 55f75dcdf3c0>)
 (#<time type: time-utc nanosecond: 0 second: 1519340400> #<ics-object VEVENT 55f75d551480>)
 (#<time type: time-utc nanosecond: 0 second: 1519254000> #<ics-object VEVENT 55f75dbb7ea0>)
 (#<time type: time-utc nanosecond: 0 second: 1518994800> #<ics-object VEVENT 55f75dba1150>)
 (#<time type: time-utc nanosecond: 0 second: 1518822000> #<ics-object VEVENT 55f75dc8e720>)
 (#<time type: time-utc nanosecond: 0 second: 1518735600> #<ics-object VEVENT 55f75d2b1f90>)
 (#<time type: time-utc nanosecond: 0 second: 1518649200> #<ics-object VEVENT 55f75d836c00>)
 (#<time type: time-utc nanosecond: 0 second: 1518562800> #<ics-object VEVENT 55f75d279900>)
 (#<time type: time-utc nanosecond: 0 second: 1518476400> #<ics-object VEVENT 55f75d4ba300>)
 (#<time type: time-utc nanosecond: 0 second: 1518390000> #<ics-object VEVENT 55f75dc0a960>) (#<time type: time-utc nanosecond: 0 second: 1518303600> #<ics-object VEVENT 55f75d30cf00>) (#<time type: time-utc nanosecond: 0 second: 1518130800> #<ics-object VEVENT 55f75d882f30>) (#<time type: time-utc nanosecond: 0 second: 1518044400> #<ics-object VEVENT 55f75dbb6810>) (#<time type: time-utc nanosecond: 0 second: 1517958000> #<ics-object VEVENT 55f75cf64660>) (#<time type: time-utc nanosecond: 0 second: 1517526000> #<ics-object VEVENT 55f75d7f87e0>) (#<time type: time-utc nanosecond: 0 second: 1517353200> #<ics-object VEVENT 55f75d8369c0>) (#<time type: time-utc nanosecond: 0 second: 1516834800> #<ics-object VEVENT 55f75d551990>) (#<time type: time-utc nanosecond: 0 second: 1516748400> #<ics-object VEVENT 55f75d787ae0>) (#<time type: time-utc nanosecond: 0 second: 1516662000> #<ics-object VEVENT 55f75dd10f60>) (#<time type: time-utc nanosecond: 0 second: 1516402800> #<ics-object VEVENT 55f75dabae70>) (#<time type: time-utc nanosecond: 0 second: 1516230000> #<ics-object VEVENT 55f75d75cc60>) (#<time type: time-utc nanosecond: 0 second: 1516143600> #<ics-object VEVENT 55f75dba13c0>) (#<time type: time-utc nanosecond: 0 second: 1515970800> #<ics-object VEVENT 55f75d790540>) (#<time type: time-utc nanosecond: 0 second: 1515625200> #<ics-object VEVENT 55f75dcdf450>) (#<time type: time-utc nanosecond: 0 second: 1515452400> #<ics-object VEVENT 55f75d836d80>) (#<time type: time-utc nanosecond: 0 second: 1515106800> #<ics-object VEVENT 55f75d8be6f0>) (#<time type: time-utc nanosecond: 0 second: 1484694000> #<ics-object VEVENT 55f75d551e10>))


((#<time type: time-utc nanosecond: 0 second: 1521154800> #<ics-object VEVENT 55f75dcdf3c0>)
 (#<time type: time-utc nanosecond: 0 second: 1519340400>
         #<ics-object VEVENT 55f75d30c7b0>
         #<ics-object VEVENT 55f75d551480>)
 (#<time type: time-utc nanosecond: 0 second: 1519254000> #<ics-object VEVENT 55f75da4a1e0> #<ics-object VEVENT 55f75dbb7ea0>)
 (#<time type: time-utc nanosecond: 0 second: 1518994800> #<ics-object VEVENT 55f75d8820c0> #<ics-object VEVENT 55f75dba1150>) (#<time type: time-utc nanosecond: 0 second: 1518822000> #<ics-object VEVENT 55f75dc8e720>) (#<time type: time-utc nanosecond: 0 second: 1518735600> #<ics-object VEVENT 55f75d2b1f90>) (#<time type: time-utc nanosecond: 0 second: 1518649200> #<ics-object VEVENT 55f75d836c00>) (#<time type: time-utc nanosecond: 0 second: 1518562800> #<ics-object VEVENT 55f75d8a3600> #<ics-object VEVENT 55f75d279900>) (#<time type: time-utc nanosecond: 0 second: 1518476400> #<ics-object VEVENT 55f75d4ba300>) (#<time type: time-utc nanosecond: 0 second: 1518390000> #<ics-object VEVENT 55f75d9bfcc0> #<ics-object VEVENT 55f75dc0a960>) (#<time type: time-utc nanosecond: 0 second: 1518303600> #<ics-object VEVENT 55f75d30cf00>) (#<time type: time-utc nanosecond: 0 second: 1518130800> #<ics-object VEVENT 55f75d882f30>) (#<time type: time-utc nanosecond: 0 second: 1518044400> #<ics-object VEVENT 55f75dbb6810>) (#<time type: time-utc nanosecond: 0 second: 1517958000> #<ics-object VEVENT 55f75dd217e0> #<ics-object VEVENT 55f75cf64660>) (#<time type: time-utc nanosecond: 0 second: 1517526000> #<ics-object VEVENT 55f75d7f87e0>) (#<time type: time-utc nanosecond: 0 second: 1517353200> #<ics-object VEVENT 55f75d8369c0>) (#<time type: time-utc nanosecond: 0 second: 1516834800> #<ics-object VEVENT 55f75d551990>) (#<time type: time-utc nanosecond: 0 second: 1516748400> #<ics-object VEVENT 55f75d787ae0>) (#<time type: time-utc nanosecond: 0 second: 1516662000> #<ics-object VEVENT 55f75dd10f60>) (#<time type: time-utc nanosecond: 0 second: 1516402800> #<ics-object VEVENT 55f75dabae70>) (#<time type: time-utc nanosecond: 0 second: 1516230000> #<ics-object VEVENT 55f75d739fc0> #<ics-object VEVENT 55f75d75cc60>) (#<time type: time-utc nanosecond: 0 second: 1516143600> #<ics-object VEVENT 55f75dba13c0>) (#<time type: time-utc nanosecond: 0 second: 1515970800> #<ics-object VEVENT 55f75d790540>) (#<time type: time-utc nanosecond: 0 second: 1515625200> #<ics-object VEVENT 55f75dcdf450>) (#<time type: time-utc nanosecond: 0 second: 1515452400> #<ics-object VEVENT 55f75d836d80>) (#<time type: time-utc nanosecond: 0 second: 1515106800> #<ics-object VEVENT 55f75d8be6f0>) (#<time type: time-utc nanosecond: 0 second: 1484694000> #<ics-object VEVENT 55f75d551e10>))


((#<time type: time-utc nanosecond: 0 second: 1521154800> #<ics-object VEVENT 55f75dcdf3c0>)
 (#<time type: time-utc nanosecond: 0 second: 1519340400> #<ics-object VEVENT 55f75d30c7b0> #<ics-object VEVENT 55f75d551480>)
 (#<time type: time-utc nanosecond: 0 second: 1519254000> #<ics-object VEVENT 55f75da4a1e0> #<ics-object VEVENT 55f75dbb7ea0>)
 (#<time type: time-utc nanosecond: 0 second: 1518994800> #<ics-object VEVENT 55f75d8820c0> #<ics-object VEVENT 55f75dba1150>)
 (#<time type: time-utc nanosecond: 0 second: 1518822000> #<ics-object VEVENT 55f75dc8e720>)
 (#<time type: time-utc nanosecond: 0 second: 1518735600> #<ics-object VEVENT 55f75d2b1f90>)
 (#<time type: time-utc nanosecond: 0 second: 1518649200> #<ics-object VEVENT 55f75d836c00>)
 (#<time type: time-utc nanosecond: 0 second: 1518562800> #<ics-object VEVENT 55f75d8a3600> #<ics-object VEVENT 55f75d279900>)
 (#<time type: time-utc nanosecond: 0 second: 1518476400> #<ics-object VEVENT 55f75d4ba300>)
 (#<time type: time-utc nanosecond: 0 second: 1518390000> #<ics-object VEVENT 55f75d9bfcc0> #<ics-object VEVENT 55f75dc0a960>)
 (#<time type: time-utc nanosecond: 0 second: 1518303600> #<ics-object VEVENT 55f75d30cf00>)
 (#<time type: time-utc nanosecond: 0 second: 1518130800> #<ics-object VEVENT 55f75d882f30>)
 (#<time type: time-utc nanosecond: 0 second: 1518044400> #<ics-object VEVENT 55f75dbb6810>)
 (#<time type: time-utc nanosecond: 0 second: 1517958000> #<ics-object VEVENT 55f75dd217e0> #<ics-object VEVENT 55f75cf64660>)
 (#<time type: time-utc nanosecond: 0 second: 1517526000> #<ics-object VEVENT 55f75d7f87e0>) (#<time type: time-utc nanosecond: 0 second: 1517353200> #<ics-object VEVENT 55f75d8369c0>) (#<time type: time-utc nanosecond: 0 second: 1516834800> #<ics-object VEVENT 55f75d551990>) (#<time type: time-utc nanosecond: 0 second: 1516748400> #<ics-object VEVENT 55f75d787ae0>) (#<time type: time-utc nanosecond: 0 second: 1516662000> #<ics-object VEVENT 55f75dd10f60>) (#<time type: time-utc nanosecond: 0 second: 1516402800> #<ics-object VEVENT 55f75dabae70>) (#<time type: time-utc nanosecond: 0 second: 1516230000> #<ics-object VEVENT 55f75d739fc0> #<ics-object VEVENT 55f75d75cc60>) (#<time type: time-utc nanosecond: 0 second: 1516143600> #<ics-object VEVENT 55f75dba13c0>) (#<time type: time-utc nanosecond: 0 second: 1515970800> #<ics-object VEVENT 55f75d790540>) (#<time type: time-utc nanosecond: 0 second: 1515625200> #<ics-object VEVENT 55f75dcdf450>) (#<time type: time-utc nanosecond: 0 second: 1515452400> #<ics-object VEVENT 55f75d836d80>) (#<time type: time-utc nanosecond: 0 second: 1515106800> #<ics-object VEVENT 55f75d8be6f0>) (#<time type: time-utc nanosecond: 0 second: 1484694000> #<ics-object VEVENT 55f75d551e10>))


(#<time type: time-utc nanosecond: 0 second: 1516230000>
        #<ics-object VEVENT 55f75d739fc0>
        #<ics-object VEVENT 55f75d75cc60>)

(#<time type: time-utc nanosecond: 0 second: 1516230000>
        #<ics-object VEVENT 55f75d739fc0>
        #<ics-object VEVENT 55f75d75cc60>)

(#<ics-object VEVENT 55f75d739fc0> #<ics-object VEVENT 55f75d75cc60>)

((#<time type: time-utc nanosecond: 0 second: 1516230000> . #<ics-object VEVENT 55f75d739fc0>)
 (#<time type: time-utc nanosecond: 0 second: 1516230000> . #<ics-object VEVENT 55f75d75cc60>))

(define *a*
  (let ((func event->time)
        (list *sevs*))
    (receive (keys alist)
        (fold-multiple
         (lambda (item keys alist)
           (let ((key (func item)))
             (values (cons key keys)
                     (acons key item alist))))
         list
         '() '())
      ;; This secound part filters out all values that have each key.
      ;; It does so one by one, and could probably do more at once.
      (map (lambda (key)
             (cons key
                   (map cdr
                        (filter (lambda (item)
                                  (equal? key (car item)))
                                alist))))
           (unique keys))))) ERRORError: retort-syntax


(define *b*
  (let ((func event->time)
        (list *sevs*))
    (group-by func list)))

*b*
((#<time type: time-utc nanosecond: 0 second: 1521154800> #<ics-object VEVENT 556f34e28d20>)
 (#<time type: time-utc nanosecond: 0 second: 1519340400> #<ics-object VEVENT 556f345d5a20> #<ics-object VEVENT 556f34c31480>)
 (#<time type: time-utc nanosecond: 0 second: 1519254000> #<ics-object VEVENT 556f348c1ba0> #<ics-object VEVENT 556f34c7e210>)
 (#<time type: time-utc nanosecond: 0 second: 1518994800> #<ics-object VEVENT 556f34b1da20> #<ics-object VEVENT 556f34e63780>)
 (#<time type: time-utc nanosecond: 0 second: 1518822000> #<ics-object VEVENT 556f349d16c0>) (#<time type: time-utc nanosecond: 0 second: 1518735600> #<ics-object VEVENT 556f3482db70>) (#<time type: time-utc nanosecond: 0 second: 1518649200> #<ics-object VEVENT 556f34e1b990>) (#<time type: time-utc nanosecond: 0 second: 1518562800> #<ics-object VEVENT 556f34988d80> #<ics-object VEVENT 556f34a63d80>) (#<time type: time-utc nanosecond: 0 second: 1518476400> #<ics-object VEVENT 556f34e59cf0>) (#<time type: time-utc nanosecond: 0 second: 1518390000> #<ics-object VEVENT 556f34c31690> #<ics-object VEVENT 556f34e58930>) (#<time type: time-utc nanosecond: 0 second: 1518303600> #<ics-object VEVENT 556f34ceab10>) (#<time type: time-utc nanosecond: 0 second: 1518130800> #<ics-object VEVENT 556f34dec870>) (#<time type: time-utc nanosecond: 0 second: 1518044400> #<ics-object VEVENT 556f349146f0>) (#<time type: time-utc nanosecond: 0 second: 1517958000> #<ics-object VEVENT 556f34dc2a20> #<ics-object VEVENT 556f34db70c0>) (#<time type: time-utc nanosecond: 0 second: 1517526000> #<ics-object VEVENT 556f34d54db0>) (#<time type: time-utc nanosecond: 0 second: 1517353200> #<ics-object VEVENT 556f34dfb9f0>) (#<time type: time-utc nanosecond: 0 second: 1516834800> #<ics-object VEVENT 556f349885a0>) (#<time type: time-utc nanosecond: 0 second: 1516748400> #<ics-object VEVENT 556f348da150>) (#<time type: time-utc nanosecond: 0 second: 1516662000> #<ics-object VEVENT 556f34e58870>) (#<time type: time-utc nanosecond: 0 second: 1516402800> #<ics-object VEVENT 556f34851300>) (#<time type: time-utc nanosecond: 0 second: 1516230000> #<ics-object VEVENT 556f34899900> #<ics-object VEVENT 556f34e63420>) (#<time type: time-utc nanosecond: 0 second: 1516143600> #<ics-object VEVENT 556f34dfbd20>) (#<time type: time-utc nanosecond: 0 second: 1515970800> #<ics-object VEVENT 556f348c1810>) (#<time type: time-utc nanosecond: 0 second: 1515625200> #<ics-object VEVENT 556f34c6dfc0>) (#<time type: time-utc nanosecond: 0 second: 1515452400> #<ics-object VEVENT 556f34b5c3c0>) (#<time type: time-utc nanosecond: 0 second: 1515106800> #<ics-object VEVENT 556f34cfc1e0>))


(with-output-to-file "/home/hugo/mnt/lys/cal/index.html"
  (lambda ()
    (displayln "<!doctype html>")
    (sxml->xml (get-sxml-doc *group-evs*))))
 #<unspecified>

 #<unspecified>

 #<unspecified>






(remainder (floor (time->decimal-hour (current-time)))
          24) 1


422065
 168826371/400

(dirname (current-filename)) 
(current-filename) #f

(dirname "guile-ics-test.scm") "."

(use-modules (web uri)
             (ice-9 rdelim))

(split-and-decode-uri-path "/file/style.css")
 ("file" "style.css")

(define (path-join lst)
  (string-join lst "/" 'infix))

(cond ((null? path)
       get-document)
      ((equal? "file" (car path))
       (call-with-input-file
           (path-join (cons "./front" (cdr path)))
         read-string))
      (else
       ;; Do stuff
       ))




(call-with-input-file ".///////front///////test.txt" read-string)
 "This is some text\n"
