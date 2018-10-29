(define-module (calendar gui html)
  #:use-module (calendar obj)
  #:use-module (calendar time)
  #:use-module (calendar format) ; css-ify
  #:use-module (calendar gui color)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (get-sxml-doc)) 

(define (vev->sxml vev)
  (let* ((color (color-by-calendar vev))
         (start-time (vevent->time "DTSTART" vev))
         (start-date (time-utc->date start-time))

         (end-time (vevent->time "DTEND" vev))
         (end-date (time-utc->date end-time))

         (duration (time-difference end-time start-time))
         (style
             (string-append
              (format #f "top: calc(100%/24 * ~a);"
                      (date->decimal-hour start-date))
              (format #f "height: calc(100%/24 * ~a);"
                      (time->decimal-hour duration)
                      ;; (date->decimal-hour duration)
                      )
              (format #f "width: calc(100% * (~a));" (get-width vev))
              (format #f "left: calc(100% * (~a) * (~a));"
                      (get-width vev)
                      (get-x vev))
              
             )))
    `(div (@ (class ,(string-join `("event" ,(css-ify (containing-calendar vev))) " " 'infix))
             (style ,style))
          ;; NOTE summary-proc temporarily disabled to allow modularization 
          #; ,(summary-proc vev)
          ,((extract "SUMMARY") vev)
          )))


;;; An event group is a list where the car is a time object,
;;; and the cdr is a list of VEVENT's
(define (event-group->sxml evgrp)
  (let* ((date (car evgrp))
         (datestr (date->string date "~1")))
    `(div (@ (class "day")
             (id ,datestr))
          (div (@ (class "meta"))
               (span (@ (class "dayname"))
                     ,(date->string date "~a"))
               (span (@ (class "daydate"))
                     ,datestr))
          (div (@ (class "events"))
               ,@ (map vev->sxml (cdr evgrp))))))

;;; Takesr a list of event groupsr
(define (get-sxml-doc evgrps calendar-names)
 `(html (head
         (title "Calendar")
         (meta (@ (charset "utf-8")))
         (link (@ (type "text/css")
                  (rel "stylesheet")
                  (href "file/style.css")))
         (script (@ (src "file/jquery-3.3.1.min.js")) "")
         (script (@ (src "file/script.js")) "")
         (style
             ,@(map (lambda (cal color)
                      (format #f ".~a { background-color: ~a; }"
                              (css-ify cal)
                              (if (symbol? (cdr color))
                                  (cdr color)
                                  (apply format #f "rgb(~a,~a,~a,0.9)"
                                         (cdr color)))))
                    calendar-names
                    (get-calendar-colors calendar-names))))
        (body (span (@ (id "gen-time"))
                    ,(date->string (current-date) "Last Updated: ~c"))
              (button (@ (id "goto-today")
                         (onclick "gotoToday()"))
                      "Goto Today")
              (div (@ (class "calendar"))
                   ,@ (map (lambda (time)
                             `(div (@ (id ,(string-append "clock-" time))
                                      (class "clock"))
                                   ,(string-append time ":00")))
                           (map number->string (iota 12 0 2)))
                   (div (@ (class "days"))
                        ,@ (map event-group->sxml evgrps))))))
