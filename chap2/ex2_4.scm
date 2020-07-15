;:
;: exercise 2.4
;: Here is an alternative procedure representation of pairs.
;:

(define (cons x y)
    (lambda (m) (m x y)))

(define (car z)
    (z (lambda (p q) p)))

(define (cdr z)
    (z (lambda (p q) q)))
