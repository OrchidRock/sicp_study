;:
;: exercise 4.28
;: 

(define (square-lazy a)
    (let ((f square))
        (f a)))
