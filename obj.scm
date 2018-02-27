(define-module (obj)
  #:use-module (oop goops)
  #:use-module (oop goops describe)

  #:use-module (ics type object)
  #:use-module (ics type property property)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (ice-9 format)
  #:export (<ics-path-object> describe-vevent get-files-in-dir slot-set-ret!))


(define-class <ics-path-object> (<ics-object>)
  (path #:getter ics-filepath)
  (x-index #:init-value 0
           #:accessor x-value)
  (width #:init-value 100
         #:setter set-width!))

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

