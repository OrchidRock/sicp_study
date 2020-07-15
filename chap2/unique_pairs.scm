;:
;: exercise 2.40
;: 
(load "conventional_interfaces.scm")
(define (unique-pairs n)
    (accumulate append
                '()
                (map (lambda(i) (map (lambda (j) (list i j))
                                (enumerate-interval 1 (- i 1))))
                     (enumerate-interval 1 n))))
