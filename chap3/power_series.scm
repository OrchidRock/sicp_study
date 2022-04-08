;:
;: exercise 3.59
;:

(load "streams_interfaces.scm")

;: a)
(define (integrate-series stream)
    (mul-stream stream (integers-recip-starting-from 1)))

;: b)
;; e^x = {1, 1, 1/2, 1/3!, 1/4!, 1/5!, ... }
(define exp-series (cons-stream 1 (integrate-series exp-series)))

;; cosx = {1, 0, -1/2, 0, 1/4!, ...}
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

;; sinx = {0, 1, 0, -1/3!, 0, 1/5!, ...}
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

;: exercise 3.60
(define (mul-series s1 s2)
    (cons-stream (* (stream-car s1) (stream-car s2))
                 (add-streams (scale-stream (stream-cdr s2)
                                            (stream-car s1))
                              (mul-series (stream-cdr s1) s2))))

(define mul-test (add-streams (mul-series sine-series sine-series)
                              (mul-series cosine-series cosine-series)))

;: exercise 3.61
(define (recip-series S)
    (cons-stream 1 (mul-series (scale-stream (stream-cdr S) -1)
                               (recip-series S))))
;: exercise 3.63
(define (recip-series2 S)
    (define series
        (cons-stream 1 (mul-series (scale-stream (stream-cdr S)
                                                  -1)
                                   series)))
    series)

;: secx = 1/cosx = {1, 0, 1/2, 0, 5/24, 0, 61/720, 0, 277/8064,...}
(define sec-series (recip-series cosine-series))
(define csc-series (recip-series sine-series))

;: exercise 3.62
(define (div-series s1 s2)
    (if (= (stream-car s2) 0)
        (error "s2 worng")
        (mul-series s1 (recip-series s2))))

;; tanx = sinx / cosx = {0,1,0,1/3,0,2/15,0,17/315,0,62/2835,...}
(define tan-series (div-series sine-series cosine-series))
