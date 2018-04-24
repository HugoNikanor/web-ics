(define-module (obj)
  #:use-module (oop goops)
  #:use-module (oop goops describe)

  #:use-module (ics type object)
  #:use-module (ics type property property)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)

  #:use-module (ice-9 format)
  #:use-module (ice-9 curried-definitions)

  #:use-module (time)

  #:export (<ics-path-object>

            ics-filepath
            get-x set-x!
            get-width set-width!
            start end elength
            containing-calendar

            describe-vevent
            get-files-in-dir
            slot-set-ret!
            extract
            filter-on-property
            ))


(define-class <ics-path-object> (<ics-object>)
  (path #:getter ics-filepath)
  (x-index #:init-value 0
           #:getter get-x
           #:setter set-x!)
  (width #:init-value 1
         #:setter set-width!
         #:getter get-width)
  (start-date #:getter start)
  (end-date #:getter end)
  (event-length #:getter elength)
  ;; Should be a string or symbol, not yet decided
  (calendar #:getter containing-calendar)
  )

(define-method (update-instance-for-different-class
                (old <ics-object>)
                (new <ics-path-object>))
  (let ((stime (slot-set! new 'start-date
                          (string->date*
                           ((extract "DTSTART") new))))
        (etime (slot-set! new 'end-date
                          (string->date*
                           ((extract "DTEND") new)))))
    (slot-set! new 'event-length
               (time-difference (date->time-utc etime)
                                (date->time-utc stime)))))

(define (describe-vevent vev)
  (let ((props (ics-object-properties vev)))
    (string-join 
     (append '("Direct Slots:" "===============")
             (map (lambda (slot-name)
                    (format #f "~10,@a: ~a"
                            slot-name
                            (slot-ref vev slot-name)))
                  (map slot-definition-name
                       (class-direct-slots (class-of vev))))
             '("Internal VEVENT:" "================")
             (map (cut format #f "~10,@a: ~a" <> <>)
                  (map ics-property-name props)
                  (map ics-property-value props)))
     "\n"
     'suffix)))

(define-method (describe (vev <ics-object>))
  (display (describe-vevent vev)))

(define-method (describe (vev <ics-path-object>))
  (display (ics-filepath vev))
  (newline)
  (next-method))

(define (slot-set-ret! obj slot value)
  "Sets value of slot, and returns the object
instead of the new value"
  (slot-set! obj slot value)
  obj)

(define ((extract field) item)
  "Get value of field in item"
  (ics-property-value
   (ics-object-property-ref item field)))

(define (filter-on-property property-name func items)
  "Filter, but only on lists of VEVENT's, and 
\(extract property-name) is run on the item before
func recieves it."
  (filter (lambda (item)
            (func ((extract property-name) item)))
          items))
