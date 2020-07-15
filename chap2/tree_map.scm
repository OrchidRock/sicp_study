;:
;: exercise 2.31
;:
;: 



(define (tree-map item tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (tree-map item sub-tree)
                (item sub-tree)))
         tree))

(define (tree-map item tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (item tree))
          (else (cons (tree-map item (car tree))
                      (tree-map item (cdr tree))))))
