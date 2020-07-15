;:
;: exercise 4.31
;:

(define (f a b c d)
    (+ a b c d))
(f (+ 1 2) (+ 2 3) (* 1 2) (square 2))

(define (f a (b lazy) c (d lazy-memo))
    (+ a b c d))
(f (+ 1 2) (+ 2 3) (* 1 2) (square 2))

(define (f a (b lazy) c (d lazy-memo))
    (+ a b b c d d))
(f (+ 1 2) (+ 2 3) (* 1 2) (square 2))
