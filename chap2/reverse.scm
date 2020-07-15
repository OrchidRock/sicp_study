;:
;: exercise 2.18
;: Define a procedure that takes a list as argument and returns a list of the same elements 
;: in reverse order.

(define nil '())
(define (reverse l)
    (define (iter l result)
        (if (null? l)
            result
            (iter (cdr l) (cons (car l) result))))
    (iter l nil))

;: exercise 2.39
;: Redefine reverse
(define (reverse sequence)
    (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse sequence)
    (fold-left (lambda (x y) (cons y x)) '() sequence))

;: test
(define x1 (list 1 4 9 16 25))
(define T2 (reverse x1))
