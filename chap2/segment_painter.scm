;#lang racket
;(require graphics/graphics)
(load "frame.scm")
(load "segment.scm")

(define (draw-line vs ve)
    (display vs)
    (display "->")
    (display ve)
    (newline))

(define (segment->painter segment-list)
    (lambda (frame)
        (for-each (lambda (segment)
                        (draw-line ((frame-coord-map frame) (start-segment segment))
                                   ((frame-coord-map frame) (end-segment segment))))
                  segment-list)))

;:
;: exercise 2.49
;: a) The painter that draws the outline of the designated frame.
(define frame-edge-painter
    (segment->painter (list (make-segment (make-vect 0 0)
                                          (make-vect 0 1))
                            (make-segment (make-vect 0 0)
                                          (make-vect 1 0))
                            (make-segment (make-vect 0 1)
                                          (make-vect 1 1))
                            (make-segment (make-vect 1 0)
                                          (make-vect 1 1)))))
;: b) The painter that draws an "X" by connecting opposite
;:    corners of the frame.
(define frame-x-painter
    (segment->painter (list (make-segment (make-vect 0 0)
                                          (make-vect 1 1))
                            (make-segment (make-vect 1 0)
                                          (make-vect 0 1)))))

;: c) The painter that draws a diamond shape by connnecting
;:    the midpoint of the sides of the frame.
(define frame-diamond-painter
    (segment->painter (list (make-segment (make-vect 0 0.5)
                                          (make-vect 0.5 0))
                            (make-segment (make-vect 0.5 0)
                                          (make-vect 1 0.5))
                            (make-segment (make-vect 1 0.5)
                                          (make-vect 0.5 1))
                            (make-segment (make-vect 0.5 1)
                                          (make-vect 0 0.5)))))

;: Test
(define frame1 (make-frame (make-vect 0 0.5)
                           (make-vect 0.5 0.5)
                           (make-vect 0.5 -0.5)))
(frame-edge-painter frame1)
(frame-x-painter frame1)
(frame-diamond-painter frame1)
