
;: exercise 1.31
;: Write an analogue procedure called product that returns the product of the values of a 
;: function at points over a given rnage.

(define (product term a next b)
    (if (> a b)
        1
        (* (term a)
           (product term (next a) next b))))

;: iterative version.
(define (product-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))

(define (factorial-new n)
    (define (identity x) x)
    (define (next x) (+ x 1))
    (product identity 1 next n))

;: load accumulate.scm to revoke another product which build on accumulate procedure.
;: load procedure will override procedures above all.
(load "accumulate.scm")
;: recursive version 
(define (pi-product b)
    (define (pi-next x) (+ x 2.0))
    (define (pi-term x) (/ (* (- x 1) (+ x 1))
                         (* x x)))
    (* 4 (product pi-term 3.0 pi-next b)))

;: iterative version
;: tail-recursive
(define (pi-product-iter b)
    (define (pi-next x) (+ x 2.0))
    (define (pi-term x) (/ (* (- x 1) (+ x 1))
                         (* x x)))
    (* 4 (product-iter pi-term 3.0 pi-next b)))

