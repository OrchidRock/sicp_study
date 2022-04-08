;:
;: exercise 3.5
;:
;#lang scheme

(load "random.scm")
(define (estimate-integral P? x1 x2 y1 y2 trials)
    (define (iter trial-remaining trial-passed)
        (cond ((= trial-remaining 0)
                (/ trial-passed trials))
              ((P? (random-in-range x1 x2)
                   (random-in-range y1 y2))
                (iter (- trial-remaining 1) (+ trial-passed 1)))
              (else
                (iter (- trial-remaining 1) trial-passed))))
    (iter trials 0))


(define (estimate-pi)
    (* 4.0 (estimate-integral (lambda (x y)
                                (<= (+ (square x) (square y)) 1))
                            -1 1 -1 1 100000)))
