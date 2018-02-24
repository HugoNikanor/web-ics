#!/usr/bin/guile \
-e main -s
!#

;;; I know that main probably should be the main file,
;;; and that the code file should probably have some
;;; other name.
;;; But I will fix that when I break main.scm up into
;;; different modules.

(add-to-load-path (string-append (dirname (current-filename))
                                 "/lib/guile-mime/"))
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
             (web response)

             (ice-9 rdelim)

             (util)
             (mime))

(define (get-document)
  (with-output-to-string
    (lambda ()
      (displayln "<!doctype html>")
      (sxml->xml (get-sxml-doc *group-evs*)))))

(define mime (get-mime-hash-table))

(define (404-page)
  (values (build-response
           #:code 404
           #:headers '((content-type text/html)))
   (with-output-to-string
     (lambda ()
       (displayln "<!doctype html>")
       (sxml->xml
        `(html (meta (@ (charset "utf-8")))
               (title "Missing File")
               (style "
body {
	display: flex;
	align-items: center;
	justify-content: center;

	margin: 0px;
	width: 100vw;
	height: 100vh;
}")

               "This page unintentionally left blank."))))))

(define (handler request body)
  (let ((path (split-and-decode-uri-path (uri-path (request-uri request)))))
    (cond ((null? path)    ; This can't be the best way to check for root
           (values '((content-type text/html))
                   (get-document)))
          ((equal? "file" (car path))
           (let ((local-path (path-join (cons "./front" (cdr path)))))
             (if (access? local-path R_OK)
                 (let* ((extension (file-extension local-path))
                        (mime-type (hash-ref mime extension)))
                   (values `((content-type ,(string->symbol mime-type)))
                           (call-with-input-file local-path read-string)))
                 (404-page))))
          (else (404-page)))))

(define (main args)
  (run-server handler))


