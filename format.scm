(define-module (format)
  ;; #:use-module ()
  #:export (strip-summary-liu))

(define (strip-summary-liu str)
  (apply (lambda (course-code type . groups)
           (format #f "[~a]: ~a" course-code type))
         (map string-trim (string-split str #\,))))

