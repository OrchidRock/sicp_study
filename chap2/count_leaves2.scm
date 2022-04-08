;:
;: exercise 2.35
;: Redefine count-leaves as an accumulate
;:
;:
(load "conventional_interfaces.scm")

(define (count-leaves2 tree)
    (accumulate (lambda (x y)
                    (if (pair? x)
                        (+ (count-leaves2 x) y)
                        (+ x y)))
                0
                (map (lambda (sub-tree)
                        (if (pair? sub-tree)
                            sub-tree
                            1))
                    tree)))


;: Test
(define x (cons (list 1 2) (list 3 4)))
(define xx (list x x))
;(define T1 (count-leaves x))
;(define T2 (count-leaves xx))
