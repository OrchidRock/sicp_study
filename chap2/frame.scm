;:
;: exercise 2.47
;:
(load "vector.scm")
(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))
(define (origin-frame f) (car f))
(define (edge-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

(define (frame-coord-map frame)
    (lambda (v)
        ;(display "v=")
        ;(display v)
        ;(display ", frame=")
        ;(display frame)
        ;(newline)
        (add-vect (origin-frame frame)
                  (add-vect (scale-vect (xcor-vect v)
                                        (edge-frame frame))
                            (scale-vect (ycor-vect v)
                                        (edge2-frame frame))))))
