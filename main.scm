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

(add-to-load-path (dirname (current-filename)))

(use-modules (ics)
             (ics type object)
             (ics type property property)

             (srfi srfi-1)              ; List library.
             (srfi srfi-19)             ; Time/Date library.
             (srfi srfi-26)             ; Specializing parameters

             (macros arrow)

             (oop goops)

             (ice-9 ftw)
             (ice-9 curried-definitions)
             (ice-9 format)
             ;; (ice-9 rdelim)
             )

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

(define cal-path
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

(define (file-extension filename)
  "Returns the file extension of a filename"
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

(define (slot-set-ret! obj slot value)
  "Sets value of slot, and returns the object
instead of the new value"
  (slot-set! obj slot value)
  obj)

;;; list of all ics-objects in filename
;;; parsed as if each file only had one VEVENT
;;; origininal path of file stored in object as well
(define ics-objs
  (map (lambda (filename)
         (-> filename
             open-input-file
             ics->scm
             car
             ics-object-components
             car
             (change-class <ics-path-object>)
             (slot-set-ret! 'path filename)))
       (get-files-in-dir cal-path "ics")))

(define (sort* items comperator get)
  "A sort function more in line with how python's sorted works"
  (sort items (lambda (a b)
                (comperator (get a)
                            (get b)))))

(define ((extract field) item)
  "Get value of field in item"
  (ics-property-value
   (ics-object-property-ref item field)))

(define (filter-on-property property-name func items)
  "Filter, but only on lists of VEVENT's, and 
(extract property-name) is run on the item before
func recieves it."
  (filter (lambda (item)
            (func ((extract property-name) item)))
          items))
