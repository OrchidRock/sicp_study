;:
;: Do some experiments to determain how many average damps are requared to 
;: compute nth roots as a fixed-point search based upon repeated average damping
;: y I-> x/(y^(n-1))

(load "fixed_point.scm")
(load "repeated_function.scm")

(define (n-average-damp f n)
    (repeated (average-damp f) n))

(define (n-roots n x)
    (define (repeated-count n)
        (cond ((<= n 3) 1)
              ((= n 4) 2)
              ((= n 5) 2)
              ((= n 18) 3)
              (else 2)))

    (fixed-point (n-average-damp (lambda (y) (/ x (expt y (- n 1)))) (repeated-count n)) 1.0))

