(define-module (calendar format)
  #:use-module (ice-9 regex)
  #:export (strip-summary-liu course-code? css-ify))

;;; Meningen är att den här ska skriva om de långa
;;; sammanfattningarna vilka TimeEdit ger mig till
;;; kortare och trevligare saker. Dock matar TimeEdit
;;; ur sig så pass jobbiga strängar att det inte
;;; fungerar.
;;; Jag tror jag ska försöka detektera om någonting
;;; är en kurskod och därifrån göra mera.
(define (strip-summary-liu str)
  (apply (lambda (course-code type . groups)
           (format #f "[~a]: ~a" course-code type))
         (map string-trim (string-split str #\,))))

(define (course-code? str)
  "Returns a regexp match if str is a course code, #f otherwise."
  (string-match "T[A-Z]{3}[0-9]{2}" str))

(define (css-ify str)
  "Makes a string a valid CSS class name, by replacing a number of
characters for hyphens '-', and prepending 'c-'."
  (let ((reserved-characters
         '(#\~ #\! #\@ #\$ #\% #\^
           #\& #\* #\( #\) #\+ #\=
           #\, #\. #\/ #\' #\; #\:
           #\" #\? #\> #\< #\[ #\]
           #\\ #\{ #\} #\| #\` #\#)))
    (string-append
     "c-" (string-map (lambda (c)
                        (if (memv c reserved-characters)
                            #\- c))
                      str))))
