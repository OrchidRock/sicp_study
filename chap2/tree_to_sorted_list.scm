;:
;: exercise 2.63
;:

(load "binary_tree.scm")

(define (cons-counter x y)
    ;(display "*")
    (cons x y))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons-counter (car list1) (append (cdr list1) list2))))

;: recursive procedure
(define (tree->list-1 tree)
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
                (cons-counter (entry tree)
                      (tree->list-1 (right-branch tree))))))

;: iterative procedure
(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        ;(display result-list)
        ;(newline)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons-counter (entry tree)
                                (copy-to-list (right-branch tree)
                                              result-list)))))
    (copy-to-list tree '()))
;: a) Do the two procedure have the same result for every tree?
;:    Answer: Yes
;: b) Do the two procedure have the same order of growth in the number of steps required to
;:    convert a balanced tree with n elements to a list?
;:    Answer: the number of cons be called is n by tree->list-2, but it will be more and
;:            more in tree->list-1 by which append be called.
;: Test
(define x1 (make-tree 7 
                      (make-tree 3
                                 (make-tree 1 '() '())
                                 (make-tree 5 '() '()))
                      (make-tree 9
                                 '()
                                 (make-tree 11 '() '()))))
(define x2 (make-tree 3 
                      (make-tree 1 '() '())
                      (make-tree 7
                                 (make-tree 5 '() '())
                                 (make-tree 9 
                                           '()
                                           (make-tree 11 '() '())))))
;(define t1 (tree->list-1 x1))
;(newline)
;(define t2 (tree->list-2 x1))
;(newline)
;(define t3 (tree->list-1 x2))
;(newline)
;(define t4 (tree->list-2 x2))
