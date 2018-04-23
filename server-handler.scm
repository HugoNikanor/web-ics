(define-module (server-handler)
  #:use-module (code)
  #:use-module (sxml simple)

  #:use-module (output line)
  #:use-module (css)

  ;; #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (web response)

  #:use-module (ice-9 rdelim)

  #:use-module (util)
  #:use-module (logger)
  #:use-module (mime)
  #:export (*default-mime* handler))

(define mime (get-mime-hash-table))

(define *default-mime* "text/plain")

(define (handler request body)
  (format #t "Incomming Request ~s\n" request)
  (let ((path (split-and-decode-uri-path (uri-path (request-uri request)))))
    (log (request-uri request))
    (cond ((null? path)    ; This can't be the best way to check for root
           (values '((content-type text/html))
                   (get-document)))
          ((equal? "file" (car path))
           (let ((local-path (path-join (cons "./front" (cdr path)))))
             (if (access? local-path R_OK)
                 (let* ((extension (file-extension local-path))
                        (mime-type (or (hash-ref mime extension)
                                       *default-mime*)))
                   (values `((content-type ,(string->symbol mime-type)))
                           (call-with-input-file local-path read-string)))
                 (404-page))))
          (else (404-page)))))

;; Returns the actual calendar page, as html
(define (get-document)
  (with-output-to-string
    (lambda ()
      (displayln "<!doctype html>")
      (sxml->xml (get-sxml-doc (get-sorted-groups))))))

;;; Returns the full 404 page, with
;;; mulitple return values and all!
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
               (style ,default-style)
               "404"
               (br)
               "This page unintentionally left blank."))))))

(define default-style "
body {
	display: flex;
	align-items: center;
	justify-content: center;

	margin: 0px;
	width: 100vw;
	height: 100vh;
}")
