(define-module (util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8) ; receive
  #:use-module (srfi srfi-26)
  #:use-module (macros arrow)
  #:export (flatten inner intersperce file-extension sort*
            fold-multiple unique group-by path-join path-join*
            pair-map test-until-success))

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

(define (path-join lst)
  (string-join lst "/" 'infix))

(define (path-join* . lst)
  (path-join lst))

(define* (sort* items comperator #:optional (get identity))
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
                 (if (not (or (member token right)
                              (member token left)))
                     (cons token left)
                     left)))
             '()
             lst))

;;; Makes a list where each element is a list, where car
;;; is (func (car list)) and cdr is all elements from list
;;; where func gave the same value.
(define (group-by func list)
  "Equivalent to elisp's seq-group-by,
also see guile's built in <partition> if only true/false
groups are desired. This also doesn't sort the keys, so
the return order of the keys is undefined."
  (receive (keys alist)
      (fold-multiple
       (lambda (item keys alist)
         (let ((key (func item)))
           (values (cons key keys)
                   (acons key item alist))))
       list
       '() '())
    ;; This secound part filters out all values that have each key.
    ;; It does so one by one, and could probably do more at once.
    (map (lambda (key)
           (cons key
                 (map cdr
                      (filter (lambda (item)
                                (equal? key (car item)))
                              alist))))
         (unique keys))))


(define (pair-map func list)
  "Like regular map, but func is given the whole cons cell
and not just the car."
  (if (null? list)
      '()
      (cons (func list)
            (pair-map func (cdr list)))))

(define-macro (test-until-success error . body)
  (if (null? body)
      `(throw 'out-of-cases)
      `(catch ,error
         (lambda () ,(car body))
         (lambda args
           (test-until-success ,error ,@(cdr body))))))
