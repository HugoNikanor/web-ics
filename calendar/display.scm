(define-module (calendar display)
  #:use-module (srfi srfi-19)            ; Time/Date library.
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 receive)
  #:use-module (calendar time)
  #:use-module (calendar types ics)
  #:use-module (calendar util)
  #:export (fix-event-widths))

;;; Takes a list on the form
;;; ((date calendar-obj ...) ...)
;;; And makes each sublist have better laid out elements.
;;; It's not perfect if there are many elements that overlap
;;; In different ways. But it works perfectly for a block
;;; schedule!
(define (fix-event-widths evs)
  (define (fix-within-day day-evs)
    (if (null? day-evs)
        #f
        (let ((ev-list day-evs))
          (receive (overlapping rest)
              (take-and-drop-while
               (lambda (next)
                 (time<? (date->time-utc (start next))
                         (date->time-utc (end (car ev-list)))))
               ev-list)
            (for-each set-x! overlapping (iota (length overlapping)))
            (for-each (cut set-width! <> (/ (length overlapping)))
                      overlapping)
            (fix-within-day rest)))))
  (for-each (compose fix-within-day cdr)
            evs))
