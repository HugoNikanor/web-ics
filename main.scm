#!/usr/bin/guile \
-e main -s
!#

(let ((here (dirname (current-filename))))
  (add-to-load-path (string-append here "/lib/guile-mime/"))
  (add-to-load-path (string-append here "/lib/guile-lib/"))
  (add-to-load-path here))

(use-modules (sxml simple)
             (calendar code)
             (calendar util))

(define *cal-root*
  (path-join* (getenv "HOME")
              ".calendars"))

(define (main args)
  (with-output-to-file "/tmp/cal.html"
    (lambda () (sxml->xml (load-calendars *cal-root*)))))
