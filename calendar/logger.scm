(define-module (calendar logger)
  #:export (*log-file* log))

(define *log-file* "/tmp/scheme-file")

(define (log item)
  (let ((log-file (open-file *log-file* "a")))
    (display item log-file)
    (newline log-file)
    (close log-file)))
