;:
;: exercise 2.32
;: 

(define (subsets s)
    (if (null? s) 
        (list '())
        (let ((rest (subsets (cdr s))))
             (append rest (map (lambda (x) (cons (car s) x))
                               rest)))))

;: Test

(define x (list 1 2 3))
(define x2 (list 1 2 3 4))
