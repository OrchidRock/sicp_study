;:
;: exercise 2.35
;: Redefine count-leaves as an accumulate
;: 
;: 
(load "conventional_interfaces.scm")

(define (count-leaves2 tree)
    (accumulate () () (map () ())))

;: Test
(define x (cons (list 1 2) (list 3 4)))
(define xx (list x x))
;(define T1 (count-leaves x))
;(define T2 (count-leaves xx))
