;:
;: exercise 3.59
;:

(load "streams.scm")

;: a)
(define (integrate-series stream)
    (mul-stream stream (integers-recip-starting-from 1)))

;: b)
(define exp-series (cons-stream 1 (integrate-series exp-series)))
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

;: exercise 3.60
(define (mul-series s1 s2) ())

;: exercise 3.61
()

;: exercise 3.62
(define (div-series s1 s2) ())
