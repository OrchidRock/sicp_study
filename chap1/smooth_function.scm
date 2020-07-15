;:
;: smooth function
;: g(x) = (f(x-dx) + f(x) + f(x+dx)) / 3

(load "repeated_function.scm")

(define dx 0.00001)

(define (smooth f)
    (lambda (x) (/ (+ (f (- x dx))
                      (f x)
                      (f (+ x dx)))
                   3.0)))

(define (n-smooth f n)
    (repeated (smooth f) n))

(define (smooth-test)
    ((smooth sin) 3))
(define (n-smooth-test)
    ((n-smooth sin 3) 3))
