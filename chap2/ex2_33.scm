;:
;: exercise 2.33
;: To complete the following definitions of some basic list-manipulation operations as
;: accumulations

(load "conventional_interfaces.scm")

(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(define (length sequence)
    (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (filter predicate sequence)
    (accumulate (lambda (x y) 
                    (if (predicate x) 
                        (cons x y)
                        y))
                '()
                sequence))

;: Test
(define x (list 1 2 3 4 5))
(define y (list 7 8 9))
(define x2 (list (list 1 2) 3 4 (list 5 6)))
(define T1 (map square x))
(define T2 (append x y))
(define T3 (length x2))
(define T4 (filter odd? x))
