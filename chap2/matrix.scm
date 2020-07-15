;:
;: exercise 2.37
;: Suppose we represent vectors v = (v<i>) as sequences of numbers, and matrices m =(m<ij>) as
;: sequences of vectors.
;:
(load "conventional_interfaces.scm")

;: v and w all are vectors.
(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (x) (dot-product x v)) m))

(define (transpose m)
    (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (x) (matrix-*-vector cols x)) m)))

;: Test
(define v1 (list 1 2 3 4))
(define v2 (list 3 4 5 6))
(define v3 (list 1 2 3))
(define m1 (list (list 1 2 3) (list 4 5 6)))
(define m2 (list (list 1 4) (list 2 5) (list 3 6)))
(define T1 (dot-product v1 v2))
(define T2 (matrix-*-vector m1 v3))
(define T3 (transpose m1))
(define T4 (matrix-*-matrix m1 m2))
