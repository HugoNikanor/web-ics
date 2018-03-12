#!/usr/bin/guile \
-e main -s
!#

(let ((here (dirname (current-filename)))
      (home (getenv "HOME")))
  (add-to-load-path (string-append here "/lib/guile-mime/"))
  (add-to-load-path (string-append home "/lib/guile/"))
  (add-to-load-path here))

(use-modules (web server)
             (server-handler))

(define (main args)
  (run-server handler 'http '(#:addr 0)))
