(define-module (util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8) ; receive
  #:use-module (srfi srfi-26)
  #:use-module (macros arrow)
  #:export (flatten inner intersperce file-extension sort*
            fold-multiple unique))

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

(define (fold-multiple func list . nils)
  (if (null? list)
      (apply values (map reverse nils))
      (call-with-values
          (lambda () (apply func
                       (cons (car list)
                             nils)))
        (cut fold-multiple func (cdr list) <...>))))

(define (unique lst)
  "Returns a list of all unique items in list, compared with memv.
Note that symbols are returned in oposite order that they are given."
  (pair-fold (lambda (pair left)
               (let ((token (car pair))
                     (right (cdr pair)))
                 (if (not (or (memv token right)
                              (memv token left)))
                     (cons token left)
                     left)))
             '()
             lst))
