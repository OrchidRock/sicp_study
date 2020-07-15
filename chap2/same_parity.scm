;:
;: exercise 2.20
;:
;: The produces +, *, and list take arbitrary numbers of arguments. One way to define such 
;: procedures is to use define with dotted-tail notation.
;:

(define (same-parity x . y)
    (define (body l)
        (cond ((null? l) '())
              ((even? (abs (- x (car l)))) (cons (car l)
                                                 (body (cdr l))))
              (else (body (cdr l)))))
    (cons x (body y)))

;: test

(define T1 (same-parity 1 2 3 4 5 6 7))
(define T2 (same-parity 2 3 4 5 6 7))
