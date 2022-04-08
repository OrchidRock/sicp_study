;;
;;
(load "streams_interfaces.scm")

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

;; it's not working
(define (solve f y0 dt)
    (define y
        (cons-stream y0 (integral (stream-map f y)
                                  y0
                                  dt)))
    y)



(define (solve f y0 dt)
    (define y (integral (delay dy) y0 dt))
    (define dy (stream-map f y))
    y)

(define test (solve (lambda (y) y) 1 0.001))

;: exercise 3.79
(define (solve-2nd a b dt y0 dy0)
    (define y (integral (delay dy) y0 dt))
    (define dy (integral (delay ddy) dy0 dt))
    (define ddy (add-streams (scale-stream dy a)
                             (scale-stream y b)))
    y)

(define (solve-2nd-common dt y0 dy0 f)
    (define y (integral (delay dy) y0 dt))
    (define dy (integral (delay ddy) dy0 dt))
    (define ddy (stream-map (lambda (x y) (f x y)) dy y))
    y)
