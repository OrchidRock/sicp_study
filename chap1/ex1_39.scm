;:
;:  J.H. Lambert's continued fraction representation of the tangent function
;:

(define (tan-cf x k)
    (define (n i) 
        (if (= i 1)
            x
            (* x x)))
    (define (d i)
        (- (* 2.0 i) 1.0))

    (define (recursive-body i)
        (if (> i k)
            0
            (/ (n i) (- (d i) (recursive-body (+ i 1))))))
    (recursive-body 1))

;: iterative version
;:

(define (tan-cf-iter x k)
    (define (n i) 
        (if (= i 1)
            x
            (* x x)))
    (define (d i)
        (- (* 2.0 i) 1.0))
    (define (iter k result)
        (if (< k 1)
            result
            (iter (- k 1) (/ (n k) (- (d k) result)))))
    (iter k 0))

;: To use cont-frac to re-define tan-cf

(load "continued_fraction.scm")
(define (tan-cf x k)
    (cont-frac (lambda (y) (if (= y 1) x (* x x)))
               (lambda (y) (- (* 2.0 y) 1.0))
                k
                -))
(define (tan-cf-iter x k)
    (cont-frac-iter (lambda (y) (if (= y 1) x (* x x)))
                    (lambda (y) (- (* 2.0 y) 1.0))
                    k
                    -))
