(define-module (format)
  #:export (strip-summary-liu))

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

