(define-module (calendar gui color)
  #:use-module (srfi srfi-41)            ; Streams
  #:use-module (macros arrow)
  #:export (get-calendar-colors color-by-calendar)
  )

(define *colors*
  '((red #xFF 0 0)
    (green 0 #xFF 0)
    (blue 0 0 #xFF)
    (purple #xFF 0 #xFF)
    (orange #xFF #xFF 0)))

(define (get-rand-color)
  (cdr (list-ref *colors* (random (length *colors*)))))

;;; These should be broken out into an HTML module

(define-stream (color-stream)
  (stream-cons (list (random #x100)
                     (random #x100)
                     (random #x100))
               (color-stream)))

(define (get-calendar-colors calendar-names)
  "Returns a list of pairs between names and colors.
cdr is either a symbol which is the name of the color,
or a list of RGB."
  (stream->list (stream-map cons (list->stream calendar-names)
                            (color-stream)))
  #;
  (map cons calendar-names
       (map car *colors*)))

(define (color-by-calendar event)
  (list (random #x100)
        (random #x100)
        (random #x100)))

#;
(define (color-by-calendar event)
  (-> *cal-root*
      get-cal-paths
      get-calendar-names
      get-calendar-colors
      (assoc-ref (containing-calendar event))))
