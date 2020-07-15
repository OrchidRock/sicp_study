;:
;: exercise 2.64
;: The following procedure converts an ordered list to a balanced binary tree.

(load "binary_tree.scm")

(define (list->tree element)
    (car (partial-tree element (length element))))

;: b)
;: ths complexity: ThThe(n)
(define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
            (let ((left-result (partial-tree elts left-size)))
                (let ((left-tree (car left-result))
                      (non-left-elts (cdr left-result))
                      (right-size (- n (+ left-size 1))))
                    (let ((this-entry (car non-left-elts))
                          (right-result (partial-tree (cdr non-left-elts) right-size)))
                        (let ((right-tree (car right-result))
                              (remaining-elts (cdr right-result)))
                            (let ((child-tree (make-tree this-entry
                                                         left-tree
                                                         right-tree)))
                                ;(display non-left-elts)
                                ;(display " **** ")
                                ;(display child-tree)
                                ;(newline)
                                (cons child-tree remaining-elts)))))))))

;: Test 
(define x1 (list 1 3 5 7 9 11))
(define x2 (list 1 3 4 5 7 9 11))
;(define T1 (list->tree x1))
;(newline)
;(define T2 (list->tree x2))
