
(declare (usual-integrations))

;:
;: Fibonacci function described by recursive process.
;:
(define (fibonacci-re n)
 (cond ((= n 0) 0)
       ((= n 1) 1)
       (else (+ (fibonacci-re (- n 1))
	         (fibonacci-re (- n 2))))))


;: Fibonacci function described by interative process.
;: version 1, time/space complexity: ThTa(n)
;:
(define (fibonacci-iter-1 n)
    (define (fib-iter a b count)
        (if (= count 0)
            b
            (fib-iter (+ a b) a (- count 1))))
    (fib-iter 1 0 n))

;: exercise 1.19
;: Fibonacci function described by interative process.
;: version 2, time/space complexity: ThTa(log(n))
;: 

(define (fibonacci-iter-2 n)
    (define (fib-iter a b p q count)
        (cond ((= count 0) b)
              ((even? count) (fib-iter a
		                               b
				                       (+ (* p p) (* q q))
				                       (+ (* q q) (* p q 2))
				                       (/ count 2)))
	          (else (fib-iter (+ (* b q) (* a q) (* a p))
	                          (+ (* b p) (* a q))
			                  p
			                  q
			                  (- count 1)))))
    (fib-iter 1 0 0 1 n))


