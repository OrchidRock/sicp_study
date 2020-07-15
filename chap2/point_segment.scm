;:
;: exercise 2.2
;: representing line segments in a plne.
;:

;: the selectors and constructor of point(x,y)
(define (make-point x y)
    (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;: the selectors and constructor of segment(start,end)
(define (make-segment start end)
    (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;:midpoint-segment that take a line segment as argument and returns its midpoint.
(define (midpoint-segment s)
    (make-point (/ (+ (x-point (start-segment s))
                      (x-point (end-segment s)))
                   2.0)
                (/ (+ (y-point (start-segment s))
                      (y-point (end-segment s)))
                   2.0)))

;: display
(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

