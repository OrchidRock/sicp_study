;:
;:
(load "binary_tree.scm")

(define (element-of-set? x set)
    (display set)
    (newline)
    (cond ((null? set) false)
          ((= x (entry set)) true)
          ((> x (entry set)) (element-of-set? x 
                                              (right-branch set)))
          (else (element-of-set? x (left-branch set)))))


(define (adjoin-set x set)
    (cond ((null? set) (make-tree x '() '()))
          ((= x (entry set)) set)
          ((< x (entry set))
            (make-tree (entry set) 
                       (adjoin-set x (left-branch set))
                       (right-branch set)))
          ((> x (entry set))
            (make-tree (entry set)
                       (left-branch set)
                       (adjoin-set x (right-branch set))))))


;: Test
(define x1 (make-tree 7 
                      (make-tree 3
                                 (make-tree 1 '() '())
                                 (make-tree 5 '() '()))
                      (make-tree 9
                                 '()
                                 (make-tree 11 '() '()))))


(define t1 (element-of-set? 5 x1))
(define t2 (element-of-set? 6 x1))
(define t3 (adjoin-set 10 x1))
(define t4 (adjoin-set 7 (adjoin-set 6 (adjoin-set 5 (adjoin-set 4 (adjoin-set 3 
                                                                    (adjoin-set 2 
                                                                        (make-tree 1 
                                                                                   '()
                                                                                   '()))))))))

