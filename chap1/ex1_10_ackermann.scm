;:
;: Ackermann function.
;: 

(declare (usual-integrations))

(define (Acker x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
	    ((= y 1) 2)
	    (else (Acker (- x 1)
	                 (Acker x (- y 1))))))

;: A(0,n) = 2*n
(define (f n) (Acker 0 n))

;: A(1,n) = 2^n
;: A(1,n) = A(0,A(0,...,A(0,2)...)) 
(define (g n) (Acker 1 n))

;: A(2,n) = 2^2^2...^2
;: A(2,n) = A(1,A(1,...,A(1,2)...))
(define (h n) (Acker 2 n))

;: (k n) = 5n^2
(define (k n) (* 5 n n))
