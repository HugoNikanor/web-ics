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
             (output line))

(define (main args)
  (with-output-to-file
      (string-append
       (dirname (current-filename))
       "/front/generated.html")
    (lambda ()
      (displayln "<!doctype html>")
      (sxml->xml (get-sxml-doc *group-evs*)))))

