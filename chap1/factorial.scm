;:
;: factorial function described by recursive process. 
;: When n is a large number, such as 100, the process' cost too large 
;: to get right value.

(declare (usual-integrations))

(define (factorial-re n)
 	(if(= n 1)
	    1
	    (* n (factorial-re (- n 1)))))

;: factorial function described by interative process.
;: Split of n is a large number, such as 100, the process also got right value. 

(define (factorial-iter n)
    (define (fact-iter product counter max-count)
       (if(> counter max-count)
	        product
	        (fact-iter (* product counter)
		               (+ counter 1)
		               max-count)))
	(fact-iter 1 1 n))


