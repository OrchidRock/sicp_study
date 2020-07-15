;: An infinite continued fraction
;: 

;: exercise 1.37
;: recursive version
(define (cont-frac n d k combiner)
    (define (recursive-body i)
        (if (> i k)
            0
            (/ (n i) (combiner (d i) (recursive-body (+ i 1))))))
    (recursive-body 1))

;: iterative version
(define (cont-frac-iter n d k combiner)
    (define (iter k result)
        (if (< k 1)
            result
            (iter (- k 1) (/ (n k) (combiner (d k) result)))))
    (iter k 0))

;: application
;: 1/fai
(define (fai)
    (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 1000 +))


;: exercise 1.38
;: ouler's e which is the base of the natural logarithms.
;:
(define (ouler-e) 
    (cont-frac-iter (lambda (i) 1.0) 
                    (lambda (i) 
                        (if (= (remainder (+ i 1) 3) 0)
                            (* 2 (/ (+ i 1) 3))
                            1))
                    1000
                    +))
