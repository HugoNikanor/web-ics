#!/usr/bin/guile \
-e main -s
!#

;;; I know that main probably should be the main file,
;;; and that the code file should probably have some
;;; other name.
;;; But I will fix that when I break main.scm up into
;;; different modules.

(add-to-load-path (string-append (getenv "HOME")
                                 "/lib/guile/"))

(add-to-load-path (dirname (current-filename)))

(use-modules (main)
             (sxml simple)

             (output line)

             (web server)
             (web request)
             (web response)
             (web uri)

             (ice-9 rdelim)
             )


;;; This handler really is horible!
(define (handler request request- body)
  ;; split-and-decode-uri-path
  (let ((path (uri-path (request-uri request))))
    (displayln path)
    (cond
      ((equal? path "/style.css")
       (values '((content-type text/css))
               (call-with-input-file "./front/style.css" read-string)))
      (else
       (values '((content-type text/html))
               (with-output-to-string
                 (lambda ()
                   (displayln "<!doctype html>")
                   (sxml->xml (get-sxml-doc *group-evs*)))))))))

(define (main args)
  (run-server handler))
