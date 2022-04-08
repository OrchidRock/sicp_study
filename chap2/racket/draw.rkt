#lang racket
(require graphics/graphics)
(open-graphics)
(require "vector.rkt")

(provide drawline close-draw)

(define WIDTH 300)
(define HEIGHT 300)
(define vp (open-viewport "TEST" WIDTH HEIGHT))
(define (drawline start stop)
  ((draw-line vp) (make-posn (* HEIGHT (ycor-vect start))
                             (* WIDTH (xcor-vect start)))
                  (make-posn (* HEIGHT (ycor-vect stop))
                             (* WIDTH (xcor-vect stop)))
                  "black")

  )
(define (close-draw) (close-viewport vp))
;(drawline (make-vect 0 0) (make-vect 0.5 0.5))