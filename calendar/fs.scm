;;; Module for file system operations.
;;; Reads calendars from disk, and extracts some information.

(define-module (calendar fs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (calendar util)
  #:export (get-cal-name get-files-in-dir))


;;; Currently calendars can only be auto detected, and all
;;; have to be direct childs of a common ancestor. Changing
;;; this shouldn't be to hard.

(define (get-cal-name calendar-path)
  (if (not (null? (scandir calendar-path (cut string=? "displayname" <>))))
      (call-with-input-file (path-join* calendar-path "displayname") read-line)
      (basename calendar-path)))


(define (get-files-in-dir path ext)
  "Returns a list of all direct children of <path> which have
the file extension <ext>"
  (map (compose (cut path-join* path <>)
                car)
       (filter (lambda (node)
                 (apply (lambda (name flags . children)
                          (string=? ext (file-extension name)))
                        node))
               (cddr (file-system-tree path)))))
