;:
;:
;(load "prefix_algebraic_expr.scm")
(load "infix_algebraic_expr.scm")
(define (deriv expr var)
    (display expr)
    (newline)
    (cond ((number? expr) 0)
          ((variable? expr) (if (same-variable? expr var) 1 0))
          ((sum? expr) (make-sum (deriv (addend expr) var)
                                 (deriv (augend expr) var)))
          ((product? expr) (make-sum (make-product (multiplier expr) 
                                                   (deriv (multiplicand expr) var))
                                     (make-product (multiplicand expr)
                                                   (deriv (multiplier expr) var))))
          ((exponentiation? expr) (make-product (exponent expr)
                                                (make-product (make-exponentiation (base expr)
                                                                                   (- (exponent expr) 1))
                                                              (deriv (base expr) var))))
          (else (error "unknown expression type -- DERIV" expr))))

;: Test
(define x1 '(+ x 3))
(define y1 '(x + 3))

(define x2 '(* x y))
(define y2 '(x * y))

(define x3 '(* (* x y) (+ x 3)))
(define y3 '((x * y) * (x + 3)))

(define x4 '(** (+ x 3) 3))
(define x5 '(** (+ (* x x) 3) 3))
(define x6 '(* x y (+ x 3)))
(define x7 '(+ x y (+ x 3)))

(define y4 '(x + 3 * (x + y + 2)))
