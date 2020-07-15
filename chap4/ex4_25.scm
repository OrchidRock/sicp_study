;:
;:
;:

(define (unless condition exceptional-value usual-value)
    (if condition exceptional-value usual-value))

;: In ordinary applicative-order Scheme, 
;: this procedure will never stop.
;: But we will get excepted vlue if it's runned in normal-order Scheme.

(define (factorial n)
    (unless (= n 1)
            (* n (factorial (- n 1)))
            1))
