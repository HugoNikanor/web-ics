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
