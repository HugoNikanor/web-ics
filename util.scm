(define-module (util)
  #:use-module (srfi srfi-1)
  #:use-module (macros arrow)
  #:export (flatten inner intersperce file-extension sort*))

(define (flatten tree)
  "Flattens a tree, same as printing the tree
and removing all internal parethese."
  (define (inner tree)
    (fold (lambda (item done)
            (if (list? item)
                (append (inner item) done)
                (cons item done)))
          '()
          tree))
  (reverse (inner tree)))

(define (intersperce collection other)
  "Makes every other element of @colletion @other"
  (-> collection
      (map (cut list other <>))
      flatten
      cdr))

(define (file-extension filename)
  "Returns the file extension of a filename"
  (last (string-split filename #\.)))

(define (sort* items comperator get)
  "A sort function more in line with how python's sorted works"
  (sort items (lambda (a b)
                (comperator (get a)
                            (get b)))))
