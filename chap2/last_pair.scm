;:
;: exercise 2.17
;: Define a procedure that returns the list that contains only the last element of
;: a given (noempty) list.
;:

(define nil '())
(define (last-pair l)
    (define (iter l element)
        (if (null? l)
            (list element)
            (iter (cdr l) (car l))))
    (iter l nil))

;: test
(define T1 (last-pair (list 23 72 149 34)))
