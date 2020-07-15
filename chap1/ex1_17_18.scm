;: 
;: To device a procedure that generates an recursive or interative process for
;: multiplying two integers in terms of adding, doubing, and halving and use a 
;: logarithmic number of steps.
;:

;: time/space complexity: ThTa(b)
;:
;(define (* a b)
;    (if (= b 0)
;        0
;        (+ a (* a (- b 1)))))

;: Now, we can do it
;: left-shift arith
(define (double a)
    (* a 2))

;: a must be a even number
;: right-shift arith
(define (halve a)
    (/ a 2))

;: recursive process
;: if we compare it with fast-expt process, we would have found the similarity of the structure.
(define (fast-product a b)
    (cond ((= b 0) 0)
          ((even? b) (double (fast-product a (halve b))))
          (else (+ a (fast-product a (+ b -1))))))

;: iterative process
;: if we also compare it with fast-expt-iter process, we would have found the similarity of 
;: the structure.

(define (fast-product-iter a b)
    (define (iter a b k)
        (cond ((= b 0) k)
              ((even? b) (iter (double a) (halve b) k))
              (else (iter a (+ b -1) (+ k a)))))
    (iter a b 0))

