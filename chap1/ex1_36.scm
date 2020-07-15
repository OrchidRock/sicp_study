;:
;: exercise 1.36
;: Modify fixed-point so that it prints the sequence of approximations it generates.


(define tolerance 0.00001)
(define (average x y) (/ (+ x y) 2))

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (display guess)
        (newline)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

;: To get the root of x^x = 1000. 
;:
;: without average damping
;: 34 steps => 4.55553227
(define (xpx1000)
    (fixed-point (lambda (y) (/ (log 1000) (log y))) 2.0))

;: average damping
;: 9 steps => 4.55553755
(define (xpx1000-ad)
    (fixed-point (lambda (y) (average y (/ (log 1000) (log y)))) 2.0))
