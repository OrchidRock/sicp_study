;: ex1.8 The cube roots by Newton's method.

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ (+ (/ x (* guess guess)) (* guess 2)) 3)))

(define (good-enough? guess old-guess)
    (< (abs (- guess old-guess)) 0.00001))

(define (curt-iter guess old-guess x)
    (if (good-enough? guess old-guess)
        guess
        (curt-iter (improve guess x)
                guess
                x)))

(define (curt x)
    (curt-iter 1.0 0.0 x))
