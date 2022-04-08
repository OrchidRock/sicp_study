;:
;: exercise 3.58
;: The expend procedure translates the rational to decimal fraction.
;:
(load "streams_interfaces.scm")

(define (expend num den radix)
    (cons-stream (quotient (* num radix) den)
                 (expend (remainder (* num radix) den) den radix)))

;: Test
;: 1/7 => 0. 142857142857...
(define t1 (expend 1 7 10))

;: 3/8 => 0.375000000000...
(define t2 (expend 3 8 10))
