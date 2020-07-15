
;:exercise 1.41

(declare (usual-integrations))

(define (double f) (lambda (x) (f (f x))))
(define (inc x) (+ x 1))

;: inc procedure will be called by 16 times.
(define (double-test)
    (((double (double double)) inc) 5))
