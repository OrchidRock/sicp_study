;:
;: exercise 2.34
;: We evaluate the polynomial An*(x^n) + An-1*(x^(n-1)) + ... + A1*x + A0
;: using a well-known algorithgm called Horner's rule, which structures the computation as
;: (...((An*x + An-1)*x + An-2)*x+...+A1)*x + A0
;: Horner's rule evaluates polynamial using fewer additions and multiplications than does the
;: straightforward method of first computing An*(x^n), then adding An-1*(x^(n-1)), and so on.

(load "conventional_interfaces.scm")

(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms)
                    (+ (* higher-terms x) this-coeff))
                0
                coefficient-sequence))


;: Test
;: 1 + 3x + 5x^3 + x^5, when x = 2
(define T1 (horner-eval 2 (list 1 3 0 5 0 1)))
