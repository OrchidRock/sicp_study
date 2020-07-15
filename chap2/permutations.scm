;:
;: To generate all the permutations of a set S.
;:

(load "nested_mapping.scm")
(load "remove_item.scm")

(define (permutations s)
    (if (null? s)
        (list '())
        (flatmap (lambda (x) (map (lambda (p) (cons x p))
                                  (permutations (remove-item x s)))) 
                 s)))

;: Test
(define x1 (list 1 2 3))
(define T1 (permutations x1))
