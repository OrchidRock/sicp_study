;:
;: exercise 2.65
;:

(load "sorted_set.scm")
(load "tree_to_sorted_list.scm")
(load "partial_tree.scm")

(define (union-tree-set set1 set2)
    (list->tree (union-set (tree->list-2 set1)
                           (tree->list-2 set2))))

(define (intersection-tree-set set1 set2)
    (list->tree (intersection-set (tree->list-2 set1)
                                  (tree->list-2 set2))))

;: Test
(define x1 (make-tree 7 
                      (make-tree 3
                                 (make-tree 1 '() '())
                                 (make-tree 5 '() '()))
                      (make-tree 9
                                 '()
                                 (make-tree 11 '() '()))))
(define x2 (list 5 6 9 10 12))
(define y2 (list->tree x2))
(define t5 (union-tree-set x1 y2))
(define t6 (intersection-tree-set x1 y2))
