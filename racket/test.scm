#lang racket
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "test" 100 100))

;((draw-viewport vp) "black")
((draw-line vp) (make-posn 1 1) (make-posn 50 50))
;(close-viewport vp)
;(close-graphics)