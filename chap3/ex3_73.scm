;; exercise 3.73

(load "streams_interfaces.scm")

(define (RC R C dt)
    (lambda (I v0)
        (add-streams (scale-stream I R)
                    (integral (scale-stream I (/ 1 C))
                              v0
                              dt))))
(define RC1 (RC 5 1 0.5))
(define T (RC1 ones 0))
