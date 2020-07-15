;: exercise 1.11 
;:
;: f(n) : n<3 f(n)=n; n>3 f(n)=f(n-1)+2f(n-2)+3f(n-3)
;: 

(declare (usual-integrations))

;: recursive process 

(define (ex1_11-re n)
    (if (< n 3)
        n
        (+ (ex1_11-re (- n 1)) 
           (* (ex1_11-re (- n 2)) 2)
           (* (ex1_11-re (- n 3)) 3))))

;: iterative process

(define (ex1_11-iter n)
    (define (iter a b c n)
        (if (= n 0)
            c
            (iter (+ a (* b 2) (* 3 c)) a b (- n 1))))
    (iter 2 1 0 n))


