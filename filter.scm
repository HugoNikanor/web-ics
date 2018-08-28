(define-module (filter)
  #:use-modules (sfri srfi-1)
  #:export (no-filter containsfilter summary-filter))

;;; This whole thing with summary filters should probably be
;;; replaced with filters that can work on any field.
;;; They should also be set in some form of config file
;;; instead of in the program source

;;; Different summary filters
(define (no-filter _) #t)
(define ((contains-filter string) summary)
  (string-contains string summary))

;;; Choose one summary filter
(define summary-filter no-filter)
;; (define summary-filter (contains-filter "THEN18"))

