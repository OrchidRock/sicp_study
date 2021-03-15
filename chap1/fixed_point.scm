;:
;: Finding fixed points of functions.
;:


(define tolerance 0.00001)
(define (average x y) (/ (+ x y) 2))

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

;: more general procedure to use fixed-point
(define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess))

(define (average-damp f)
    (lambda (x) (average x (f x))))

;: this fixed-point search does not converge.
(define (sqrt x)
    (fixed-point (lambda (y) (/ x y)) 1.0))

;: the converge version.
;: A technique that we call average damping, often aids the convergence of fixed-point searches.
;:
(define (sqrt x)
    (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;: the approximated value of fai
;: exercise 1.35
(define (fai)
    (fixed-point (lambda (y) (+ 1.0 (/ 1.0 y))) 1.0))

;:
(define (sqrt x)
    (fixed-point-of-transform (lambda (y) (/ x y))
                             average-damp
                             1.0))
