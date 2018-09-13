#!/usr/bin/guile \
-e main -s
!#

(let ((here (dirname (current-filename))))
  (add-to-load-path (string-append here "/lib/guile-mime/"))
  (add-to-load-path (string-append here "/lib/guile-lib/"))
  (add-to-load-path here))

(use-modules (web server)
             (server-handler)
             (sxml simple)
             (code))

(define (main args)
  #; (run-server handler 'http '(#:addr 0))
  (with-output-to-file "/tmp/cal.html"
    (lambda () (sxml->xml (do-stuff)))))
