;:
;: exercise 1.43
;: we can form the nth repeated application of f
;: f(f(f...f(x)...)))

(load "compose_functions.scm")

(define (repeated f n)
    (if (= 1 n)
        (lambda (x) (f x))
        (compose f (repeated f (- n 1)))))

(define (repeated-test)
    ((repeated square 2) 5))
