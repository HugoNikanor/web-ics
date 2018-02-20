;;; The plan with this program is to create web based calendar
;;; viewor of a vdir directory, as created by vdirsyncer, and
;;; which can also be read by khal.
;;;
;;; Currently I'm working on getting ICS files properly read
;;; and the correct data extracted from them.
;;;
;;; What really needs to be done is find some easy way
;;; to map|filter|<anything> on one field of a struct,
;;; while letting the other just follow along for the ride.

(add-to-load-path (string-append (getenv "HOME")
                                 "/lib/guile/"))

(use-modules (ics)
             (ics type object)
             (ics type property property)

             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-26)

             (macros arrow)

             (ice-9 ftw)
             (ice-9 curried-definitions)
             ;; (ice-9 rdelim)
             )


(define cal-path
  (string-append
   (getenv "HOME")
   "/.calendars/b85ba2e9-18aa-4451-91bb-b52da930e977/"))

(define (handle-ft-node node)
  (apply (lambda (name flags . children)
           (case (stat:type flags)
             ((directory) (map (compose (cut string-append name <>)
                                        handle-ft-node)
                               children))
             ((regular) name)))
         node))

(define (file-extension filename)
  (last (string-split filename #\.)))


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

(define fname-obj-list
  (map (lambda (filename)
         (list filename
               (-> filename
                   open-input-file
                   ics->scm
                   car
                   ics-object-components
                   car)))
       (get-files-in-dir cal-path "ics")))


(define (sort* items comperator get)
  (sort items (lambda (a b)
                (comperator (get a)
                            (get b)))))

(define ((extract field) item)
  (ics-property-value
   (ics-object-property-ref item field)))

;;; This probably work, but since I think my event list
;;; is litered with timezone items I can't really test it
;;; right now.
(define (map-on-property property-name func items)
  "Maps <func> over the property-field <property-name> on the
object in tha cadr of each element of items."
  (map list
       (map car items)
       (map (compose
             func
             (extract property-name)
             cadr)
            items)))

(define (filter-on-property property-name func items)
  (filter (lambda (item)
            (apply (lambda (head tail)
                     (and=> (func tail)
                            (cut list head <>)))
                   item))
          (map list
               (map car items)
               (map (compose (extract property-name)
                             cadr)
                    items))))

;;; SUMMARY är tydligen kort-beskrivning, och inte förklaring

(map (compose ;; ics-property-value
      (cut ics-object-property-ref <> "SEQUENCE")
      cadr)
     fname-obj-list)

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


fname-obj-listq
