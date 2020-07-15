;:
;: exercise 2.30
;:

(define (square-tree tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree (car tree))
                      (square-tree (cdr tree))))))


;: Test
(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree sub-tree)
                (square sub-tree)))
         tree))

;: 
(load "tree_map.scm")
(define (square-tree tree) (tree-map square tree))
